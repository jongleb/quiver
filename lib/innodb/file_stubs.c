#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* A tablespace file written and read by whole pages from any thread: each call
   copies the pages outside the OCaml heap before releasing the runtime, so the
   bytes cannot move under the system call. Results are 0 or -errno. */

#define PAGE 16384

value quiver_innodb_open(value path)
{
  int fd = open(String_val(path), O_RDWR | O_CREAT | O_TRUNC | O_CLOEXEC, 0640);
  return Val_long(fd < 0 ? -errno : fd);
}

value quiver_innodb_write(value fd, value pages, value number)
{
  CAMLparam3(fd, pages, number);
  size_t length = caml_string_length(pages);
  char *copy = malloc(length);
  if (copy == NULL) CAMLreturn(Val_long(-ENOMEM));
  memcpy(copy, Bytes_val(pages), length);
  int f = Int_val(fd);
  off_t at = (off_t)Long_val(number) * PAGE;
  long result = 0;
  caml_enter_blocking_section();
  size_t done = 0;
  while (done < length) {
    ssize_t n = pwrite(f, copy + done, length - done, at + (off_t)done);
    if (n < 0) {
      if (errno == EINTR) continue;
      result = -errno;
      break;
    }
    done += (size_t)n;
  }
  caml_leave_blocking_section();
  free(copy);
  CAMLreturn(Val_long(result));
}

value quiver_innodb_read(value fd, value pages, value number)
{
  CAMLparam3(fd, pages, number);
  size_t length = caml_string_length(pages);
  char *copy = calloc(1, length);
  if (copy == NULL) CAMLreturn(Val_long(-ENOMEM));
  int f = Int_val(fd);
  off_t at = (off_t)Long_val(number) * PAGE;
  long result = 0;
  caml_enter_blocking_section();
  size_t done = 0;
  while (done < length) {
    ssize_t n = pread(f, copy + done, length - done, at + (off_t)done);
    if (n < 0) {
      if (errno == EINTR) continue;
      result = -errno;
      break;
    }
    if (n == 0) break;
    done += (size_t)n;
  }
  caml_leave_blocking_section();
  if (result == 0) memcpy(Bytes_val(pages), copy, length);
  free(copy);
  CAMLreturn(Val_long(result));
}

value quiver_innodb_resize(value fd, value pages)
{
  return Val_long(ftruncate(Int_val(fd), (off_t)Long_val(pages) * PAGE) < 0 ? -errno : 0);
}

value quiver_innodb_close(value fd)
{
  close(Int_val(fd));
  return Val_unit;
}

#include <caml/alloc.h>
#include <sys/stat.h>

/* Small whole files (a template, its .cfg): read or written at once without
   releasing the runtime. */

value quiver_innodb_size(value path)
{
  struct stat st;
  return Val_long(stat(String_val(path), &st) < 0 ? -errno : (long)st.st_size);
}

value quiver_innodb_load(value path, value bytes)
{
  int fd = open(String_val(path), O_RDONLY | O_CLOEXEC);
  if (fd < 0) return Val_long(-errno);
  size_t length = caml_string_length(bytes), done = 0;
  while (done < length) {
    ssize_t n = read(fd, (char *)Bytes_val(bytes) + done, length - done);
    if (n < 0) {
      if (errno == EINTR) continue;
      long e = -errno;
      close(fd);
      return Val_long(e);
    }
    if (n == 0) break;
    done += (size_t)n;
  }
  close(fd);
  return Val_long(done == length ? 0 : -EIO);
}

value quiver_innodb_put(value path, value text)
{
  int fd = open(String_val(path), O_WRONLY | O_CREAT | O_TRUNC | O_CLOEXEC, 0640);
  if (fd < 0) return Val_long(-errno);
  size_t length = caml_string_length(text), done = 0;
  while (done < length) {
    ssize_t n = write(fd, String_val(text) + done, length - done);
    if (n < 0) {
      if (errno == EINTR) continue;
      long e = -errno;
      close(fd);
      return Val_long(e);
    }
    done += (size_t)n;
  }
  return Val_long(close(fd) < 0 ? -errno : 0);
}

value quiver_innodb_rename(value from, value to)
{
  return Val_long(rename(String_val(from), String_val(to)) < 0 ? -errno : 0);
}

value quiver_innodb_remove(value path)
{
  return Val_long(unlink(String_val(path)) < 0 ? -errno : 0);
}

/* The file takes the owner and group of the directory it lies in: the
   server's, when quiver runs as root; nothing to do when they are already
   the same. */
value quiver_innodb_adopt(value path)
{
  struct stat file, dir;
  if (stat(String_val(path), &file) < 0) return Val_long(-errno);
  char *copy = strdup(String_val(path));
  if (copy == NULL) return Val_long(-ENOMEM);
  char *slash = strrchr(copy, '/');
  if (slash != NULL) *slash = '\0';
  int r = stat(slash != NULL ? copy : ".", &dir);
  free(copy);
  if (r < 0) return Val_long(-errno);
  if (file.st_uid == dir.st_uid && file.st_gid == dir.st_gid) return Val_long(0);
  return Val_long(chown(String_val(path), dir.st_uid, dir.st_gid) < 0 ? -errno : 0);
}
