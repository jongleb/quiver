#define CAML_NAME_SPACE
#include <stdint.h>
#include <string.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <openssl/evp.h>

/* FF1 of NIST SP 800-38G over radix 2, and keyed permutations built on it.

   A number is an array of 64-bit limbs, least significant first, on the stack.
   FF1 takes a string of [n] bits, 2 <= n <= FF1_MAX_BITS, read as the number it
   writes in binary. A permutation of [0, N) runs FF1 on the fewest bits that
   hold N - 1 and walks the cycle (Black and Rogaway) until the result is below
   N again.

   The stubs read OCaml strings only while nothing allocates on the OCaml heap,
   and copy every result into a fresh string at the end. Each thread keeps one
   AES context and re-keys it only when a call brings another key. */

#define FF1_MAX_BITS 8192
#define LIMBS (FF1_MAX_BITS / 64 + 2)
#define MAX_TWEAK 64
#define MAX_B (FF1_MAX_BITS / 2 / 8 + 1)
#define MAX_D (4 * ((MAX_B + 3) / 4) + 4)
#define MAX_Q (MAX_TWEAK + 16 + 1 + MAX_B)

static EVP_CIPHER_CTX *keyed(const unsigned char *key, size_t length)
{
  static _Thread_local EVP_CIPHER_CTX *ctx = NULL;
  static _Thread_local unsigned char current[32];
  static _Thread_local size_t current_length = 0;
  const EVP_CIPHER *cipher =
    length == 16 ? EVP_aes_128_ecb() : length == 24 ? EVP_aes_192_ecb() : length == 32 ? EVP_aes_256_ecb() : NULL;
  if (cipher == NULL) return NULL;
  if (ctx == NULL) ctx = EVP_CIPHER_CTX_new();
  if (ctx == NULL) return NULL;
  if (current_length != length || memcmp(current, key, length) != 0) {
    if (!EVP_EncryptInit_ex(ctx, cipher, NULL, key, NULL)) {
      current_length = 0;
      return NULL;
    }
    EVP_CIPHER_CTX_set_padding(ctx, 0);
    memcpy(current, key, length);
    current_length = length;
  }
  return ctx;
}

static int block(EVP_CIPHER_CTX *ctx, const unsigned char *in, unsigned char *out)
{
  int written = 0;
  return EVP_EncryptUpdate(ctx, out, &written, in, 16) && written == 16;
}

static int bitlength(const uint64_t *x, int l)
{
  int i;
  for (i = l - 1; i >= 0; i--)
    if (x[i] != 0) return i * 64 + 64 - __builtin_clzll(x[i]);
  return 0;
}

static void truncate_bits(uint64_t *x, int l, int k)
{
  int i;
  for (i = 0; i < l; i++) {
    int low = i * 64;
    if (low >= k) x[i] = 0;
    else if (k - low < 64) x[i] &= ((uint64_t)1 << (k - low)) - 1;
  }
}

static void shift_right(const uint64_t *x, int l, int k, uint64_t *out)
{
  int q = k / 64, r = k % 64, i;
  for (i = 0; i < l; i++) {
    uint64_t low = i + q < l ? x[i + q] : 0;
    uint64_t high = i + q + 1 < l ? x[i + q + 1] : 0;
    out[i] = r == 0 ? low : (low >> r) | (high << (64 - r));
  }
}

static void join(const uint64_t *a, const uint64_t *b, int l, int k, uint64_t *out)
{
  int q = k / 64, r = k % 64, i;
  for (i = 0; i < l; i++) {
    uint64_t here = i - q >= 0 ? a[i - q] : 0;
    uint64_t below = i - q - 1 >= 0 ? a[i - q - 1] : 0;
    out[i] = (r == 0 ? here : (here << r) | (below >> (64 - r))) | b[i];
  }
}

static void add(uint64_t *a, const uint64_t *y, int l)
{
  unsigned __int128 carry = 0;
  int i;
  for (i = 0; i < l; i++) {
    carry += (unsigned __int128)a[i] + y[i];
    a[i] = (uint64_t)carry;
    carry >>= 64;
  }
}

static int compare(const uint64_t *a, const uint64_t *b, int l)
{
  int i;
  for (i = l - 1; i >= 0; i--)
    if (a[i] != b[i]) return a[i] < b[i] ? -1 : 1;
  return 0;
}

static void to_bytes(const uint64_t *x, int l, int length, unsigned char *out)
{
  int j;
  for (j = 0; j < length; j++) {
    int k = length - 1 - j;
    out[j] = k / 8 < l ? (unsigned char)(x[k / 8] >> ((k % 8) * 8)) : 0;
  }
}

static void of_bytes(const unsigned char *in, int length, uint64_t *x, int l)
{
  int j;
  memset(x, 0, sizeof(uint64_t) * l);
  for (j = 0; j < length; j++) {
    int k = length - 1 - j;
    if (k / 8 < l) x[k / 8] |= (uint64_t)in[j] << ((k % 8) * 8);
  }
}

static int ff1(EVP_CIPHER_CTX *ctx, const unsigned char *tweak, int t, int n, const uint64_t *x, int l,
               uint64_t *out)
{
  int u = n / 2, v = n - u;
  int b = (v + 7) / 8;
  int d = 4 * ((b + 3) / 4) + 4;
  int pad = ((-(t + b + 1)) % 16 + 16) % 16;
  int length = t + pad + 1 + b;
  unsigned char p[16] = {1, 2, 1, 0, 0, 2, 10, (unsigned char)(u & 0xff),
                         (unsigned char)(n >> 24), (unsigned char)(n >> 16), (unsigned char)(n >> 8), (unsigned char)n,
                         (unsigned char)(t >> 24), (unsigned char)(t >> 16), (unsigned char)(t >> 8), (unsigned char)t};
  unsigned char first[16], r[16], counter[16], q[MAX_Q], s[MAX_D + 16];
  uint64_t a[LIMBS], bb[LIMBS], c[LIMBS], y[LIMBS];
  int i, j, off;
  if (!block(ctx, p, first)) return 0;
  shift_right(x, l, v, a);
  memcpy(bb, x, sizeof(uint64_t) * l);
  truncate_bits(bb, l, v);
  for (i = 0; i < 10; i++) {
    int m = i % 2 == 0 ? u : v;
    memcpy(q, tweak, t);
    memset(q + t, 0, pad);
    q[t + pad] = (unsigned char)i;
    to_bytes(bb, l, b, q + t + pad + 1);
    memcpy(r, first, 16);
    for (off = 0; off < length; off += 16) {
      for (j = 0; j < 16; j++) r[j] ^= q[off + j];
      if (!block(ctx, r, r)) return 0;
    }
    memcpy(s, r, 16);
    for (j = 1; 16 * j < d; j++) {
      memcpy(counter, r, 16);
      counter[15] ^= (unsigned char)j;
      counter[14] ^= (unsigned char)(j >> 8);
      if (!block(ctx, counter, s + 16 * j)) return 0;
    }
    of_bytes(s, d, y, l);
    memcpy(c, a, sizeof(uint64_t) * l);
    add(c, y, l);
    truncate_bits(c, l, m);
    memcpy(a, bb, sizeof(uint64_t) * l);
    memcpy(bb, c, sizeof(uint64_t) * l);
  }
  join(a, bb, l, v, out);
  return 1;
}

static int permute(EVP_CIPHER_CTX *ctx, const unsigned char *tweak, int t, const uint64_t *limit,
                   const uint64_t *x, int l, uint64_t *out)
{
  int n = bitlength(limit, l);
  uint64_t current[LIMBS];
  memcpy(out, x, sizeof(uint64_t) * l);
  if (n == 0) return 1;
  if (n < 2) n = 2;
  do {
    memcpy(current, out, sizeof(uint64_t) * l);
    if (!ff1(ctx, tweak, t, n, current, l, out)) return 0;
  } while (compare(out, limit, l) > 0);
  return 1;
}

value quiver_aes_block(value key, value text)
{
  CAMLparam2(key, text);
  CAMLlocal1(result);
  unsigned char out[16];
  EVP_CIPHER_CTX *ctx =
    keyed((const unsigned char *)String_val(key), caml_string_length(key));
  if (ctx == NULL || caml_string_length(text) != 16
      || !block(ctx, (const unsigned char *)String_val(text), out))
    CAMLreturn(Val_none);
  result = caml_alloc_initialized_string(16, (const char *)out);
  CAMLreturn(caml_alloc_some(result));
}

value quiver_permute_int64(value key, value limit, value x)
{
  CAMLparam3(key, limit, x);
  uint64_t bound[2] = {(uint64_t)Int64_val(limit), 0};
  uint64_t number[2] = {(uint64_t)Int64_val(x), 0};
  uint64_t out[2];
  static const unsigned char none[1] = {0};
  EVP_CIPHER_CTX *ctx =
    keyed((const unsigned char *)String_val(key), caml_string_length(key));
  if (ctx == NULL || !permute(ctx, none, 0, bound, number, 2, out))
    caml_raise_out_of_memory();
  CAMLreturn(caml_copy_int64((int64_t)out[0]));
}

value quiver_permute_digits(value key, value tweak, value radixes, value digits)
{
  CAMLparam4(key, tweak, radixes, digits);
  CAMLlocal1(result);
  size_t count = caml_string_length(digits), i;
  size_t t = caml_string_length(tweak);
  uint64_t limit[LIMBS], number[LIMBS], out[LIMBS];
  unsigned char spelled[FF1_MAX_BITS];
  size_t bits = 0;
  int l;
  EVP_CIPHER_CTX *ctx;
  if (caml_string_length(radixes) != count || t > MAX_TWEAK) CAMLreturn(Val_none);
  for (i = 0; i < count && bits <= FF1_MAX_BITS + 64; i++) bits += Byte_u(radixes, i) == 0 ? 9 : 8;
  l = bits / 64 + 2 < LIMBS ? (int)(bits / 64 + 2) : LIMBS;
  memset(limit, 0, sizeof limit);
  memset(number, 0, sizeof number);
  limit[0] = 1;
  for (i = 0; i < count; i++) {
    uint64_t radix = Byte_u(radixes, i) == 0 ? 256 : Byte_u(radixes, i);
    uint64_t digit = Byte_u(digits, i);
    unsigned __int128 carry = 0;
    int j;
    if (radix < 2 || digit >= radix) CAMLreturn(Val_none);
    for (j = 0; j < l; j++) {
      carry += (unsigned __int128)limit[j] * radix;
      limit[j] = (uint64_t)carry;
      carry >>= 64;
    }
    carry = digit;
    for (j = 0; j < l; j++) {
      carry += (unsigned __int128)number[j] * radix;
      number[j] = (uint64_t)carry;
      carry >>= 64;
    }
    if (bitlength(limit, l) > FF1_MAX_BITS) CAMLreturn(Val_none);
  }
  {
    uint64_t minus_one[LIMBS];
    memset(minus_one, 0xff, sizeof minus_one);
    add(limit, minus_one, l);
  }
  ctx = keyed((const unsigned char *)String_val(key), caml_string_length(key));
  if (ctx == NULL || !permute(ctx, (const unsigned char *)String_val(tweak), (int)t, limit, number, l, out))
    caml_raise_out_of_memory();
  for (i = count; i > 0; i--) {
    uint64_t radix = Byte_u(radixes, i - 1) == 0 ? 256 : Byte_u(radixes, i - 1);
    uint64_t rest = 0;
    int j;
    for (j = l - 1; j >= 0; j--) {
      uint64_t high = (rest << 32) | (out[j] >> 32);
      uint64_t low = ((high % radix) << 32) | (out[j] & 0xffffffffu);
      out[j] = ((high / radix) << 32) | (low / radix);
      rest = low % radix;
    }
    spelled[i - 1] = (unsigned char)rest;
  }
  result = caml_alloc_initialized_string(count, (const char *)spelled);
  CAMLreturn(caml_alloc_some(result));
}
