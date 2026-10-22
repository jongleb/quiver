#define CAML_NAME_SPACE
#include <stddef.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#if defined(__linux__)
#include <linux/tcp.h>
#elif defined(__APPLE__)
#include <netinet/tcp.h>
#endif
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

/* What the kernel counted for one TCP connection, as six integers:
   [0] what follows: 0 nothing, 1 Linux times, 2 the peer's window;
   [1] bytes sent, retransmissions included; [2] bytes retransmitted;
   [3] Linux: microseconds the connection had data to send, macOS: bytes the
   peer lets it have in flight; [4] and [5] Linux: microseconds of those the
   peer's receive window, or the connection's own send buffer, held it back.
   Linux needs 4.19 for the byte counts, which the size returned tells. */
value quiver_tcp_strain(value fd)
{
  CAMLparam1(fd);
  CAMLlocal1(out);
  long values[6] = {0, 0, 0, 0, 0, 0};
#if defined(__linux__)
  struct tcp_info info;
  socklen_t size = sizeof(info);
  if (getsockopt(Int_val(fd), IPPROTO_TCP, TCP_INFO, &info, &size) == 0
      && size >= offsetof(struct tcp_info, tcpi_bytes_retrans)
                   + sizeof(info.tcpi_bytes_retrans)) {
    values[0] = 1;
    values[1] = (long)info.tcpi_bytes_sent;
    values[2] = (long)info.tcpi_bytes_retrans;
    values[3] = (long)info.tcpi_busy_time;
    values[4] = (long)info.tcpi_rwnd_limited;
    values[5] = (long)info.tcpi_sndbuf_limited;
  }
#elif defined(__APPLE__) && defined(TCP_CONNECTION_INFO)
  struct tcp_connection_info info;
  socklen_t size = sizeof(info);
  if (getsockopt(Int_val(fd), IPPROTO_TCP, TCP_CONNECTION_INFO, &info, &size) == 0) {
    values[0] = 2;
    values[1] = (long)info.tcpi_txbytes;
    values[2] = (long)info.tcpi_txretransmitbytes;
    values[3] = (long)info.tcpi_snd_wnd;
  }
#endif
  out = caml_alloc_tuple(6);
  for (int i = 0; i < 6; i++) Store_field(out, i, Val_long(values[i]));
  CAMLreturn(out);
}
