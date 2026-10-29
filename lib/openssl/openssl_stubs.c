#define CAML_NAME_SPACE
#include <pthread.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <openssl/err.h>
#include <openssl/evp.h>
#include <openssl/pem.h>
#include <openssl/rsa.h>
#include <openssl/ssl.h>
#include <openssl/x509.h>
#include <openssl/x509v3.h>

/* Memory and threads.

   Contexts and sessions live outside the OCaml heap; custom blocks hold their
   pointers and free them when collected.

   A context is configured completely inside [quiver_tls_client] or
   [quiver_tls_mutual] and only read afterwards, which OpenSSL allows from any
   thread at once.

   A session carries its own mutex. A call that may wait for the socket first
   releases the OCaml runtime, then takes the mutex, and gives both back in the
   other order, so threads may share a session. While the runtime is released
   no stub touches an OCaml block that can move: data goes through a malloc'd
   buffer, or straight into a bigarray, whose data never moves and which the
   caller keeps alive as a parameter. The [_nonblocking] transfers never wait,
   so they keep the runtime and use OCaml's bytes in place; a thread waiting
   for the mutex then holds the runtime only while another finishes a call
   that does not wait either.

   OpenSSL's error queue belongs to the calling thread, so [quiver_tls_error]
   reads what the same thread's last failed call left. */

#define Context_val(v) (*((SSL_CTX **)Data_custom_val(v)))

static void quiver_context_finalize(value v)
{
  SSL_CTX_free(Context_val(v));
}

static struct custom_operations quiver_context_ops = {
    "quiver.tls.context",    quiver_context_finalize,  custom_compare_default,
    custom_hash_default,     custom_serialize_default, custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

/* TLS 1.3 with AES-GCM must change keys long before 2^24.5 full records
   (RFC 8446, 5.5); OpenSSL does not on its own, so a session changes its
   sending keys once this many bytes, or [rekey_after] when set, went out. The
   change is asked for right before a write, which then carries the KeyUpdate
   message: asked for after the last write, it would wait for a write that
   never comes, and close_notify would go out under keys the peer lacks. */
#define QUIVER_REKEY_BYTES (1LL << 36)

struct quiver_session {
  SSL *ssl;
  pthread_mutex_t lock;
  long long written;    /* Since the last key update. */
  long long rekey_after;
  long updates;         /* Key updates sent. */
};

#define Session_val(v) (*((struct quiver_session **)Data_custom_val(v)))

static void quiver_session_finalize(value v)
{
  struct quiver_session *session = Session_val(v);
  SSL_free(session->ssl);
  pthread_mutex_destroy(&session->lock);
  free(session);
}

static struct custom_operations quiver_session_ops = {
    "quiver.tls.session",    quiver_session_finalize,  custom_compare_default,
    custom_hash_default,     custom_serialize_default, custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

static value quiver_ok(value contents)
{
  CAMLparam1(contents);
  CAMLlocal1(result);
  result = caml_alloc_small(1, 0);
  Field(result, 0) = contents;
  CAMLreturn(result);
}

static value quiver_error(int failure)
{
  value result = caml_alloc_small(1, 1);
  Field(result, 0) = Val_int(failure);
  return result;
}

static value quiver_context(SSL_CTX *ctx)
{
  CAMLparam0();
  CAMLlocal1(block);
  block = caml_alloc_custom(&quiver_context_ops, sizeof(SSL_CTX *), 0, 1);
  Context_val(block) = ctx;
  CAMLreturn(quiver_ok(block));
}

value quiver_tls_error(value unit)
{
  unsigned long code = ERR_get_error();
  char text[256];
  ERR_clear_error();
  if (code == 0) return caml_copy_string("");
  ERR_error_string_n(code, text, sizeof(text));
  return caml_copy_string(text);
}

/* A statically linked OpenSSL looks for roots where the build machine kept
   them, so system roots also come from the first bundle of this list that
   exists, unless SSL_CERT_FILE or SSL_CERT_DIR name them (the list is Go's). */
static const char *const quiver_bundles[] = {
    "/etc/ssl/certs/ca-certificates.crt",
    "/etc/pki/tls/certs/ca-bundle.crt",
    "/etc/ssl/ca-bundle.pem",
    "/etc/pki/tls/cacert.pem",
    "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem",
    "/etc/ssl/cert.pem",
    NULL};

static int quiver_system_roots(SSL_CTX *ctx)
{
  const char *const *bundle = quiver_bundles;
  if (SSL_CTX_set_default_verify_paths(ctx) != 1) return 0;
  if (getenv("SSL_CERT_FILE") != NULL || getenv("SSL_CERT_DIR") != NULL) return 1;
  while (*bundle != NULL
         && (access(*bundle, R_OK) != 0
             || SSL_CTX_load_verify_locations(ctx, *bundle, NULL) != 1))
    bundle++;
  ERR_clear_error();
  return 1;
}

/* [trust] is a [Tls.trust]: the constant [System_roots] or the block
   [Ca_file path]. Returns [Ok context] or [Error 0] for OpenSSL's failure. */
value quiver_tls_client(value trust)
{
  CAMLparam1(trust);
  SSL_CTX *ctx = SSL_CTX_new(TLS_client_method());
  int loaded;
  if (ctx == NULL) CAMLreturn(quiver_error(0));
  SSL_CTX_set_mode(ctx, SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER | SSL_MODE_AUTO_RETRY);
  loaded = SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION)
           && SSL_CTX_set_max_proto_version(ctx, TLS1_3_VERSION)
           && (Is_block(trust)
                   ? SSL_CTX_load_verify_locations(ctx, String_val(Field(trust, 0)), NULL) == 1
                   : quiver_system_roots(ctx));
  if (!loaded) {
    SSL_CTX_free(ctx);
    CAMLreturn(quiver_error(0));
  }
  CAMLreturn(quiver_context(ctx));
}

/* TLS 1.3 only; both sides present [certificate] (a PEM chain) and [key] and
   require the peer's to chain to [peers]. Returns [Ok context] or the
   constant constructor of [Tls.unusable]: 0 the context, 1 the certificate,
   2 the key, 3 key and certificate differ, 4 the trusted certificates. */
value quiver_tls_mutual(value certificate, value key, value peers)
{
  CAMLparam3(certificate, key, peers);
  SSL_CTX *ctx = SSL_CTX_new(TLS_method());
  int failure = -1;
  if (ctx == NULL) CAMLreturn(quiver_error(0));
  SSL_CTX_set_mode(ctx, SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER | SSL_MODE_AUTO_RETRY);
  if (!SSL_CTX_set_min_proto_version(ctx, TLS1_3_VERSION)
      || !SSL_CTX_set_max_proto_version(ctx, TLS1_3_VERSION))
    failure = 0;
  else if (SSL_CTX_use_certificate_chain_file(ctx, String_val(certificate)) <= 0)
    failure = 1;
  else if (SSL_CTX_use_PrivateKey_file(ctx, String_val(key), SSL_FILETYPE_PEM) <= 0)
    failure = 2;
  else if (!SSL_CTX_check_private_key(ctx))
    failure = 3;
  else if (SSL_CTX_load_verify_locations(ctx, String_val(peers), NULL) != 1)
    failure = 4;
  if (failure >= 0) {
    SSL_CTX_free(ctx);
    CAMLreturn(quiver_error(failure));
  }
  SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER | SSL_VERIFY_FAIL_IF_NO_PEER_CERT, NULL);
  CAMLreturn(quiver_context(ctx));
}

/* Returns [Ok session] or [Error 0]. */
value quiver_tls_session(value context, value fd)
{
  CAMLparam2(context, fd);
  CAMLlocal1(block);
  struct quiver_session *session = malloc(sizeof(struct quiver_session));
  if (session == NULL) CAMLreturn(quiver_error(0));
  session->ssl = SSL_new(Context_val(context));
  if (session->ssl == NULL || !SSL_set_fd(session->ssl, Int_val(fd))) {
    SSL_free(session->ssl);
    free(session);
    CAMLreturn(quiver_error(0));
  }
  pthread_mutex_init(&session->lock, NULL);
  session->written = 0;
  session->rekey_after = QUIVER_REKEY_BYTES;
  session->updates = 0;
  block = caml_alloc_custom_mem(&quiver_session_ops,
                                sizeof(struct quiver_session *),
                                sizeof(struct quiver_session));
  Session_val(block) = session;
  CAMLreturn(quiver_ok(block));
}

/* [expectation] is the constant constructor of [Tls.expectation]: 0 the
   certificate must name the host, 1 it must name the IP address, 2 the host is
   sent in SNI. Returns 0 when OpenSSL took it, else -5. Called before the
   handshake, while only the creating thread has the session. */
value quiver_tls_expect(value v, value expectation, value name)
{
  SSL *ssl = Session_val(v)->ssl;
  int taken;
  switch (Int_val(expectation)) {
  case 0:
    taken = X509_VERIFY_PARAM_set1_host(SSL_get0_param(ssl), String_val(name), 0);
    break;
  case 1:
    taken = X509_VERIFY_PARAM_set1_ip_asc(SSL_get0_param(ssl), String_val(name));
    break;
  default:
    taken = SSL_set_tlsext_host_name(ssl, String_val(name));
  }
  return Val_long(taken == 1 ? 0 : -5);
}

/* Results of a transfer or a handshake step: a count (0 for a finished
   handshake or shutdown), or minus one of 1 want read, 2 want write, 3 the
   peer closed TLS, 4 the socket failed or closed, 5 OpenSSL failed. */
static intnat quiver_outcome(SSL *ssl, int returned)
{
  switch (SSL_get_error(ssl, returned)) {
  case SSL_ERROR_WANT_READ: return -1;
  case SSL_ERROR_WANT_WRITE: return -2;
  case SSL_ERROR_ZERO_RETURN: return -3;
  case SSL_ERROR_SYSCALL: return -4;
  default: return -5;
  }
}

/* [step] 0 connects, 1 accepts, 2 sends close_notify. */
value quiver_tls_step(value v, value step)
{
  CAMLparam1(v);
  struct quiver_session *session = Session_val(v);
  int kind = Int_val(step), returned;
  intnat outcome;
  caml_enter_blocking_section();
  pthread_mutex_lock(&session->lock);
  ERR_clear_error();
  returned = kind == 0   ? SSL_connect(session->ssl)
             : kind == 1 ? SSL_accept(session->ssl)
                         : SSL_shutdown(session->ssl);
  outcome = (kind == 2 && returned >= 0) || returned == 1
                ? 0
                : quiver_outcome(session->ssl, returned);
  pthread_mutex_unlock(&session->lock);
  caml_leave_blocking_section();
  CAMLreturn(Val_long(outcome));
}

#define QUIVER_CHUNK 65536

/* On a non-blocking socket SSL_read and SSL_write never wait, so these two keep
   the runtime and move bytes straight between OCaml's buffer and OpenSSL: no
   collection can run, and the block stays where it is. Another thread may hold
   the session's mutex only for a call that does not wait either. */
value quiver_tls_read_nonblocking(value v, value bytes, value pos, value len)
{
  struct quiver_session *session = Session_val(v);
  int length = Long_val(len) < INT32_MAX ? Long_val(len) : INT32_MAX;
  int returned;
  intnat outcome;
  pthread_mutex_lock(&session->lock);
  ERR_clear_error();
  returned = SSL_read(session->ssl, Bytes_val(bytes) + Long_val(pos), length);
  outcome = returned > 0 ? returned : quiver_outcome(session->ssl, returned);
  pthread_mutex_unlock(&session->lock);
  return Val_long(outcome);
}

value quiver_tls_write_nonblocking(value v, value text, value pos, value len)
{
  struct quiver_session *session = Session_val(v);
  int length = Long_val(len) < QUIVER_CHUNK ? Long_val(len) : QUIVER_CHUNK;
  int returned;
  intnat outcome;
  pthread_mutex_lock(&session->lock);
  ERR_clear_error();
  if (session->written >= session->rekey_after
      && SSL_version(session->ssl) == TLS1_3_VERSION
      && SSL_key_update(session->ssl, SSL_KEY_UPDATE_NOT_REQUESTED) == 1) {
    session->written = 0;
    session->updates++;
  }
  returned = SSL_write(session->ssl, String_val(text) + Long_val(pos), length);
  outcome = returned > 0 ? returned : quiver_outcome(session->ssl, returned);
  if (returned > 0) session->written += returned;
  pthread_mutex_unlock(&session->lock);
  return Val_long(outcome);
}

value quiver_tls_rekey_after(value v, value bytes)
{
  struct quiver_session *session = Session_val(v);
  pthread_mutex_lock(&session->lock);
  session->rekey_after = Long_val(bytes);
  pthread_mutex_unlock(&session->lock);
  return Val_unit;
}

value quiver_tls_key_updates(value v)
{
  struct quiver_session *session = Session_val(v);
  long updates;
  pthread_mutex_lock(&session->lock);
  updates = session->updates;
  pthread_mutex_unlock(&session->lock);
  return Val_long(updates);
}

value quiver_tls_read_bigstring(value v, value bigstring, value pos, value len)
{
  CAMLparam2(v, bigstring);
  struct quiver_session *session = Session_val(v);
  char *target = (char *)Caml_ba_data_val(bigstring) + Long_val(pos);
  int length = Long_val(len) < INT32_MAX ? Long_val(len) : INT32_MAX;
  int returned;
  intnat outcome;
  caml_enter_blocking_section();
  pthread_mutex_lock(&session->lock);
  ERR_clear_error();
  returned = SSL_read(session->ssl, target, length);
  outcome = returned > 0 ? returned : quiver_outcome(session->ssl, returned);
  pthread_mutex_unlock(&session->lock);
  caml_leave_blocking_section();
  CAMLreturn(Val_long(outcome));
}

value quiver_tls_write(value v, value text, value pos, value len)
{
  CAMLparam2(v, text);
  struct quiver_session *session = Session_val(v);
  int length = Long_val(len) < QUIVER_CHUNK ? Long_val(len) : QUIVER_CHUNK;
  char *buffer = malloc(length > 0 ? length : 1);
  int returned;
  intnat outcome;
  if (buffer == NULL) caml_raise_out_of_memory();
  memcpy(buffer, String_val(text) + Long_val(pos), length);
  caml_enter_blocking_section();
  pthread_mutex_lock(&session->lock);
  ERR_clear_error();
  returned = SSL_write(session->ssl, buffer, length);
  outcome = returned > 0 ? returned : quiver_outcome(session->ssl, returned);
  pthread_mutex_unlock(&session->lock);
  caml_leave_blocking_section();
  free(buffer);
  CAMLreturn(Val_long(outcome));
}

static X509 *quiver_peer(struct quiver_session *session)
{
  X509 *certificate;
  caml_enter_blocking_section();
  pthread_mutex_lock(&session->lock);
  certificate = SSL_get1_peer_certificate(session->ssl);
  pthread_mutex_unlock(&session->lock);
  caml_leave_blocking_section();
  return certificate;
}

value quiver_tls_verify_result(value v)
{
  CAMLparam1(v);
  struct quiver_session *session = Session_val(v);
  long result;
  caml_enter_blocking_section();
  pthread_mutex_lock(&session->lock);
  result = SSL_get_verify_result(session->ssl);
  pthread_mutex_unlock(&session->lock);
  caml_leave_blocking_section();
  CAMLreturn(Val_long(result));
}

value quiver_tls_verify_message(value code)
{
  return caml_copy_string(X509_verify_cert_error_string(Long_val(code)));
}

/* The SHA-256 of the peer certificate's DER, or "" without a certificate. */
value quiver_tls_peer_sha256(value v)
{
  CAMLparam1(v);
  CAMLlocal1(result);
  unsigned char digest[EVP_MAX_MD_SIZE];
  unsigned int length = 0;
  X509 *certificate = quiver_peer(Session_val(v));
  if (certificate == NULL) CAMLreturn(caml_copy_string(""));
  if (!X509_digest(certificate, EVP_sha256(), digest, &length)) length = 0;
  X509_free(certificate);
  result = caml_alloc_string(length);
  memcpy(Bytes_val(result), digest, length);
  CAMLreturn(result);
}

/* The constant constructor of [Tls.validity]: 0 the peer certificate is valid
   now, 1 it is not yet or no longer valid, 2 there is none. */
value quiver_tls_peer_validity(value v)
{
  CAMLparam1(v);
  X509 *certificate = quiver_peer(Session_val(v));
  time_t now;
  int current;
  if (certificate == NULL) CAMLreturn(Val_int(2));
  now = time(NULL);
  current = ASN1_TIME_cmp_time_t(X509_get0_notBefore(certificate), now) < 0
            && ASN1_TIME_cmp_time_t(X509_get0_notAfter(certificate), now) > 0;
  X509_free(certificate);
  CAMLreturn(Val_int(current ? 0 : 1));
}

/* [algorithm] 0 SHA-1, 1 SHA-256. */
value quiver_sha(value algorithm, value text)
{
  CAMLparam1(text);
  CAMLlocal1(result);
  unsigned char digest[EVP_MAX_MD_SIZE];
  unsigned int length = 0;
  if (!EVP_Digest(String_val(text), caml_string_length(text), digest, &length,
                  Int_val(algorithm) == 0 ? EVP_sha1() : EVP_sha256(), NULL))
    caml_raise_out_of_memory();
  result = caml_alloc_string(length);
  memcpy(Bytes_val(result), digest, length);
  CAMLreturn(result);
}

/* HMAC-SHA256 under [key] (at most one 64-byte block) of the 8-byte
   big-endian [prefix] followed by [text]. Each thread keeps its own digest
   context, so no lock is needed; [key] and [text] are read before the one
   OCaml allocation. */
value quiver_hmac_sha256(value key, value prefix, value text)
{
  CAMLparam3(key, prefix, text);
  CAMLlocal1(result);
  static _Thread_local EVP_MD *md = NULL;
  static _Thread_local EVP_MD_CTX *ctx = NULL;
  unsigned char pad[64], inner[32], outer[32], head[8];
  unsigned int length = 0;
  size_t key_length = caml_string_length(key);
  uint64_t number = (uint64_t)Int64_val(prefix);
  int i, ok;
  if (md == NULL) md = EVP_MD_fetch(NULL, "SHA256", NULL);
  if (ctx == NULL) ctx = EVP_MD_CTX_new();
  if (md == NULL || ctx == NULL || key_length > sizeof pad)
    caml_raise_out_of_memory();
  for (i = 0; i < 8; i++) head[i] = (unsigned char)(number >> (56 - 8 * i));
  memset(pad, 0x36, sizeof pad);
  for (i = 0; i < (int)key_length; i++) pad[i] ^= (unsigned char)Byte_u(key, i);
  ok = EVP_DigestInit_ex(ctx, md, NULL)
       && EVP_DigestUpdate(ctx, pad, sizeof pad)
       && EVP_DigestUpdate(ctx, head, sizeof head)
       && EVP_DigestUpdate(ctx, String_val(text), caml_string_length(text))
       && EVP_DigestFinal_ex(ctx, inner, &length);
  memset(pad, 0x5c, sizeof pad);
  for (i = 0; i < (int)key_length; i++) pad[i] ^= (unsigned char)Byte_u(key, i);
  ok = ok && EVP_DigestInit_ex(ctx, md, NULL)
       && EVP_DigestUpdate(ctx, pad, sizeof pad)
       && EVP_DigestUpdate(ctx, inner, sizeof inner)
       && EVP_DigestFinal_ex(ctx, outer, &length);
  if (!ok) caml_raise_out_of_memory();
  result = caml_alloc_string(sizeof outer);
  memcpy(Bytes_val(result), outer, sizeof outer);
  CAMLreturn(result);
}

/* No OCaml allocation happens while [pem] and [text] are read, so the
   pointers stay valid. */
value quiver_rsa_encrypt(value pem, value text)
{
  CAMLparam2(pem, text);
  CAMLlocal1(result);
  BIO *bio = BIO_new_mem_buf(String_val(pem), caml_string_length(pem));
  EVP_PKEY *key = bio == NULL ? NULL : PEM_read_bio_PUBKEY(bio, NULL, NULL, NULL);
  EVP_PKEY_CTX *context = key == NULL ? NULL : EVP_PKEY_CTX_new(key, NULL);
  const unsigned char *input = (const unsigned char *)String_val(text);
  size_t input_length = caml_string_length(text);
  unsigned char *output = NULL;
  size_t length = 0;
  int done = context != NULL
             && EVP_PKEY_encrypt_init(context) > 0
             && EVP_PKEY_CTX_set_rsa_padding(context, RSA_PKCS1_OAEP_PADDING) > 0
             && EVP_PKEY_encrypt(context, NULL, &length, input, input_length) > 0
             && (output = malloc(length)) != NULL
             && EVP_PKEY_encrypt(context, output, &length, input, input_length) > 0;
  EVP_PKEY_CTX_free(context);
  EVP_PKEY_free(key);
  BIO_free(bio);
  if (!done) {
    free(output);
    ERR_clear_error();
    CAMLreturn(Val_none);
  }
  result = caml_alloc_string(length);
  memcpy(Bytes_val(result), output, length);
  free(output);
  CAMLreturn(caml_alloc_some(result));
}
