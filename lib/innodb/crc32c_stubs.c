#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <stdint.h>
#include <string.h>

/* CRC-32C (Castagnoli), as InnoDB checksums pages. Hardware instructions where
   the target has them, otherwise a table. */

#if defined(__aarch64__) && defined(__ARM_FEATURE_CRC32)
#include <arm_acle.h>

static uint32_t crc32c(uint32_t c, const uint8_t *p, size_t n)
{
  while (n >= 8) {
    uint64_t v;
    memcpy(&v, p, 8);
    c = __crc32cd(c, v);
    p += 8;
    n -= 8;
  }
  while (n--) c = __crc32cb(c, *p++);
  return c;
}

#else

static uint32_t table[256];
static int table_ready;

static uint32_t crc32c_soft(uint32_t c, const uint8_t *p, size_t n)
{
  if (!table_ready) {
    for (uint32_t i = 0; i < 256; i++) {
      uint32_t x = i;
      for (int k = 0; k < 8; k++) x = (x >> 1) ^ (0x82F63B78u & (0u - (x & 1)));
      table[i] = x;
    }
    table_ready = 1;
  }
  while (n--) c = table[(c ^ *p++) & 0xFF] ^ (c >> 8);
  return c;
}

#if defined(__x86_64__)
#include <nmmintrin.h>

__attribute__((target("sse4.2"))) static uint32_t crc32c_sse(uint32_t c, const uint8_t *p, size_t n)
{
  uint64_t x = c;
  while (n >= 8) {
    uint64_t v;
    memcpy(&v, p, 8);
    x = _mm_crc32_u64(x, v);
    p += 8;
    n -= 8;
  }
  c = (uint32_t)x;
  while (n--) c = _mm_crc32_u8(c, *p++);
  return c;
}

static uint32_t crc32c(uint32_t c, const uint8_t *p, size_t n)
{
  return __builtin_cpu_supports("sse4.2") ? crc32c_sse(c, p, n) : crc32c_soft(c, p, n);
}
#else
static uint32_t crc32c(uint32_t c, const uint8_t *p, size_t n)
{
  return crc32c_soft(c, p, n);
}
#endif
#endif

/* Does not allocate, so [bytes] cannot move while it is read; the OCaml side
   checks the range first. */
intnat quiver_crc32c(value bytes, intnat pos, intnat len)
{
  return (intnat)(~crc32c(0xFFFFFFFFu, (const uint8_t *)Bytes_val(bytes) + pos, (size_t)len));
}

value quiver_crc32c_byte(value bytes, value pos, value len)
{
  return Val_long(quiver_crc32c(bytes, Long_val(pos), Long_val(len)));
}
