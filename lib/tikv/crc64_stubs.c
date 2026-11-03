#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <stdint.h>
#include <stddef.h>

/* CRC-64/XZ (ECMA-182 reflected), eight bytes a step over eight tables, as
   crc64fast computes TiKV's table checksums. The tables are filled once when
   the program loads, before any thread reads them. */

static uint64_t tables[8][256];

__attribute__((constructor)) static void fill(void)
{
  for (uint64_t i = 0; i < 256; i++) {
    uint64_t c = i;
    for (int k = 0; k < 8; k++) c = (c >> 1) ^ (0xC96C5795D7870F42ull & (0ull - (c & 1)));
    tables[0][i] = c;
  }
  for (int k = 1; k < 8; k++)
    for (int i = 0; i < 256; i++)
      tables[k][i] = (tables[k - 1][i] >> 8) ^ tables[0][tables[k - 1][i] & 0xFF];
}

static uint64_t update(uint64_t c, const uint8_t *p, size_t n)
{
  while (n >= 8) {
    uint64_t v = (uint64_t)p[0] | (uint64_t)p[1] << 8 | (uint64_t)p[2] << 16 | (uint64_t)p[3] << 24 |
                 (uint64_t)p[4] << 32 | (uint64_t)p[5] << 40 | (uint64_t)p[6] << 48 | (uint64_t)p[7] << 56;
    c ^= v;
    c = tables[7][c & 0xFF] ^ tables[6][(c >> 8) & 0xFF] ^ tables[5][(c >> 16) & 0xFF] ^
        tables[4][(c >> 24) & 0xFF] ^ tables[3][(c >> 32) & 0xFF] ^ tables[2][(c >> 40) & 0xFF] ^
        tables[1][(c >> 48) & 0xFF] ^ tables[0][c >> 56];
    p += 8;
    n -= 8;
  }
  while (n--) c = tables[0][(c ^ *p++) & 0xFF] ^ (c >> 8);
  return c;
}

/* Does not allocate, so neither string can move while it is read. */
int64_t quiver_crc64_pair(value key, value data)
{
  uint64_t c = update(~0ull, (const uint8_t *)String_val(key), caml_string_length(key));
  return (int64_t)~update(c, (const uint8_t *)String_val(data), caml_string_length(data));
}

value quiver_crc64_pair_byte(value key, value data)
{
  return caml_copy_int64(quiver_crc64_pair(key, data));
}
