#define CAML_NAME_SPACE
#define XXH_INLINE_ALL
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include "xxhash.h"

/* A digest state lives outside the OCaml heap (XXH3 wants it 64-byte aligned);
   the block holds its pointer and frees it when collected. */

#define State_val(v) (*((XXH3_state_t **)Data_custom_val(v)))

static void quiver_xxh3_finalize(value v)
{
  XXH3_freeState(State_val(v));
}

static struct custom_operations quiver_xxh3_ops = {
    "quiver.xxh3",           quiver_xxh3_finalize,     custom_compare_default,
    custom_hash_default,     custom_serialize_default, custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

value quiver_xxh3_create(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(v);
  XXH3_state_t *state = XXH3_createState();
  if (state == NULL) caml_raise_out_of_memory();
  XXH3_64bits_reset(state);
  v = caml_alloc_custom_mem(&quiver_xxh3_ops, sizeof(XXH3_state_t *),
                            sizeof(XXH3_state_t));
  State_val(v) = state;
  CAMLreturn(v);
}

/* The following do not allocate, so [bytes] cannot move while they read it;
   the OCaml side checks the range first. */

value quiver_xxh3_add(value v, value bytes, value pos, value len)
{
  XXH3_64bits_update(State_val(v), Bytes_val(bytes) + Long_val(pos),
                     Long_val(len));
  return Val_unit;
}

int64_t quiver_xxh3_value(value v)
{
  return (int64_t)XXH3_64bits_digest(State_val(v));
}

value quiver_xxh3_value_byte(value v)
{
  return caml_copy_int64(quiver_xxh3_value(v));
}

int64_t quiver_xxh3_digest(value bytes, intnat pos, intnat len)
{
  return (int64_t)XXH3_64bits(Bytes_val(bytes) + pos, len);
}

value quiver_xxh3_digest_byte(value bytes, value pos, value len)
{
  return caml_copy_int64(
      quiver_xxh3_digest(bytes, Long_val(pos), Long_val(len)));
}
