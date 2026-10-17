#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <zstd.h>
#include <zstd_errors.h>

/* No stub here allocates on the OCaml heap or releases the runtime lock while
   it holds a pointer into an OCaml block, so the blocks cannot move under it.
   The OCaml side checks every offset and length before the call. Errors come
   back as the negated zstd error code. */

static intnat quiver_zstd_result(size_t code)
{
  return ZSTD_isError(code) ? -(intnat)ZSTD_getErrorCode(code) : (intnat)code;
}

value quiver_zstd_min_level(value unit)
{
  return Val_int(ZSTD_minCLevel());
}

value quiver_zstd_max_level(value unit)
{
  return Val_int(ZSTD_maxCLevel());
}

value quiver_zstd_bound(value length)
{
  return Val_long(ZSTD_compressBound(Long_val(length)));
}

value quiver_zstd_error_message(value code)
{
  return caml_copy_string(ZSTD_getErrorString((ZSTD_ErrorCode)Long_val(code)));
}

/* One frame with its checksum: a fresh context per call, as each render thread
   compresses its own batches. */
value quiver_zstd_compress(value level, value source, value length,
                           value target, value at, value room)
{
  ZSTD_CCtx *context = ZSTD_createCCtx();
  size_t code;
  if (context == NULL) caml_raise_out_of_memory();
  code = ZSTD_CCtx_setParameter(context, ZSTD_c_compressionLevel, Int_val(level));
  if (!ZSTD_isError(code))
    code = ZSTD_CCtx_setParameter(context, ZSTD_c_checksumFlag, 1);
  if (!ZSTD_isError(code))
    code = ZSTD_compress2(context, Bytes_val(target) + Long_val(at),
                          Long_val(room), Bytes_val(source), Long_val(length));
  ZSTD_freeCCtx(context);
  return Val_long(quiver_zstd_result(code));
}

/* A decompression context lives outside the OCaml heap; the block holds its
   pointer and frees it when collected. */

#define Dctx_val(v) (*((ZSTD_DCtx **)Data_custom_val(v)))

static void quiver_zstd_finalize(value v)
{
  ZSTD_freeDCtx(Dctx_val(v));
}

static struct custom_operations quiver_zstd_ops = {
    "quiver.zstd.dctx",      quiver_zstd_finalize,     custom_compare_default,
    custom_hash_default,     custom_serialize_default, custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

value quiver_zstd_inflater(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(v);
  ZSTD_DCtx *context = ZSTD_createDCtx();
  if (context == NULL) caml_raise_out_of_memory();
  v = caml_alloc_custom_mem(&quiver_zstd_ops, sizeof(ZSTD_DCtx *),
                            ZSTD_sizeof_DCtx(context));
  Dctx_val(v) = context;
  CAMLreturn(v);
}

/* Decompresses from [source] at [from] up to [until] into [target] at [at] up
   to [limit]; stores how far the source and the target got in fields 0 and 1
   of [progress] and returns zstd's hint (0 once a frame ended) or an error. */
value quiver_zstd_inflate(value inflater, value source, value from, value until,
                          value target, value at, value limit, value progress)
{
  ZSTD_inBuffer in = {Bytes_val(source), Long_val(until), Long_val(from)};
  ZSTD_outBuffer out = {Bytes_val(target) + Long_val(at),
                        Long_val(limit) - Long_val(at), 0};
  size_t code = ZSTD_decompressStream(Dctx_val(inflater), &out, &in);
  Field(progress, 0) = Val_long(in.pos);
  Field(progress, 1) = Val_long(Long_val(at) + out.pos);
  return Val_long(quiver_zstd_result(code));
}

value quiver_zstd_inflate_byte(value *argv, int argn)
{
  return quiver_zstd_inflate(argv[0], argv[1], argv[2], argv[3], argv[4],
                             argv[5], argv[6], argv[7]);
}

value quiver_zstd_compress_byte(value *argv, int argn)
{
  return quiver_zstd_compress(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5]);
}
