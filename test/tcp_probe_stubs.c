#define CAML_NAME_SPACE
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

/* Reads back what Tcp.tune set, in the units it documents (bytes, seconds);
   -1 where the platform has no such option. */

static int quiver_get(int fd, int level, int name)
{
  int value = -1;
  socklen_t size = sizeof(value);
  if (getsockopt(fd, level, name, &value, &size) != 0) return -1;
  return value;
}

value quiver_tcp_probe(value fd)
{
  CAMLparam1(fd);
  CAMLlocal1(settings);
  int socket = Int_val(fd);
  int unacknowledged = -1, idle = -1, unsent = -1;
#ifdef TCP_NOTSENT_LOWAT
  unsent = quiver_get(socket, IPPROTO_TCP, TCP_NOTSENT_LOWAT);
#endif
#ifdef TCP_KEEPIDLE
  idle = quiver_get(socket, IPPROTO_TCP, TCP_KEEPIDLE);
#elif defined(TCP_KEEPALIVE)
  idle = quiver_get(socket, IPPROTO_TCP, TCP_KEEPALIVE);
#endif
#ifdef TCP_USER_TIMEOUT
  unacknowledged = quiver_get(socket, IPPROTO_TCP, TCP_USER_TIMEOUT) / 1000;
#elif defined(TCP_RXT_CONNDROPTIME)
  unacknowledged = quiver_get(socket, IPPROTO_TCP, TCP_RXT_CONNDROPTIME);
#endif
  settings = caml_alloc_tuple(7);
  Store_field(settings, 0, Val_int(quiver_get(socket, IPPROTO_TCP, TCP_NODELAY) != 0));
  Store_field(settings, 1, Val_int(unsent));
  Store_field(settings, 2, Val_int(quiver_get(socket, SOL_SOCKET, SO_KEEPALIVE) != 0));
  Store_field(settings, 3, Val_int(idle));
  Store_field(settings, 4, Val_int(quiver_get(socket, IPPROTO_TCP, TCP_KEEPINTVL)));
  Store_field(settings, 5, Val_int(quiver_get(socket, IPPROTO_TCP, TCP_KEEPCNT)));
  Store_field(settings, 6, Val_int(unacknowledged));
  CAMLreturn(settings);
}

/* The IP_TOS of the socket, or -1. */
value quiver_tcp_probe_tos(value fd)
{
  return Val_int(quiver_get(Int_val(fd), IPPROTO_IP, IP_TOS));
}
