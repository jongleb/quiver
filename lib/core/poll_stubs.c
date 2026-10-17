#define CAML_NAME_SPACE
#include <errno.h>
#include <poll.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

/* Touches no OCaml block while the runtime lock is released: the descriptor,
   direction and timeout are copied into C locals first. Returns the constant
   constructor of [Blocking.outcome]: 0 ready, 1 expired, 2 interrupted; any
   other failure counts as ready, so the next read or write reports it. */

value quiver_poll(value fd, value direction, value milliseconds)
{
  struct pollfd target = {
      .fd = Int_val(fd),
      .events = Int_val(direction) == 0 ? POLLIN : POLLOUT,
      .revents = 0};
  int timeout = Int_val(milliseconds);
  int ready, failure;
  caml_enter_blocking_section();
  ready = poll(&target, 1, timeout);
  failure = errno;
  caml_leave_blocking_section();
  if (ready > 0) return Val_int(0);
  if (ready == 0) return Val_int(1);
  return Val_int(failure == EINTR ? 2 : 0);
}
