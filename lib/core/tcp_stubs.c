#define CAML_NAME_SPACE
#include <stdio.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#ifdef __APPLE__
#include <sys/sysctl.h>
#endif
#include <caml/mlvalues.h>

/* Settings for every TCP connection quiver makes or accepts. Each is best
   effort: a kernel without one keeps its default and the connection works.

   - At most QUIVER_UNSENT bytes wait in the kernel unsent, so a writer blocks
     early instead of filling memory with frames the network has not taken.
   - A silent peer is found by keepalive after QUIVER_IDLE seconds and
     QUIVER_PROBES probes QUIVER_INTERVAL seconds apart, instead of the
     kernel's two hours.
   - Data unacknowledged for QUIVER_UNACKNOWLEDGED seconds ends the connection,
     instead of the ~15 minutes of retransmissions; a peer that acknowledges
     with a zero window (busy committing) is alive. */

#define QUIVER_UNSENT (512 * 1024)
#define QUIVER_IDLE 30
#define QUIVER_INTERVAL 10
#define QUIVER_PROBES 6
#define QUIVER_UNACKNOWLEDGED 120

static void quiver_set(int fd, int level, int name, int value)
{
  (void)setsockopt(fd, level, name, &value, sizeof(value));
}

value quiver_tcp_tune(value fd)
{
  int socket = Int_val(fd);
  quiver_set(socket, IPPROTO_TCP, TCP_NODELAY, 1);
#ifdef TCP_NOTSENT_LOWAT
  quiver_set(socket, IPPROTO_TCP, TCP_NOTSENT_LOWAT, QUIVER_UNSENT);
#endif
  quiver_set(socket, SOL_SOCKET, SO_KEEPALIVE, 1);
#ifdef TCP_KEEPIDLE
  quiver_set(socket, IPPROTO_TCP, TCP_KEEPIDLE, QUIVER_IDLE);
#elif defined(TCP_KEEPALIVE)
  quiver_set(socket, IPPROTO_TCP, TCP_KEEPALIVE, QUIVER_IDLE);
#endif
#ifdef TCP_KEEPINTVL
  quiver_set(socket, IPPROTO_TCP, TCP_KEEPINTVL, QUIVER_INTERVAL);
#endif
#ifdef TCP_KEEPCNT
  quiver_set(socket, IPPROTO_TCP, TCP_KEEPCNT, QUIVER_PROBES);
#endif
#ifdef TCP_USER_TIMEOUT
  quiver_set(socket, IPPROTO_TCP, TCP_USER_TIMEOUT, QUIVER_UNACKNOWLEDGED * 1000);
#elif defined(TCP_RXT_CONNDROPTIME)
  quiver_set(socket, IPPROTO_TCP, TCP_RXT_CONNDROPTIME, QUIVER_UNACKNOWLEDGED);
#endif
  return Val_unit;
}

/* The connection's smoothed round trip time in microseconds (milliseconds
   on macOS, scaled), or -1 where the kernel does not tell. */
value quiver_tcp_rtt(value fd)
{
#if defined(TCP_INFO) && defined(__linux__)
  struct tcp_info info;
  socklen_t size = sizeof(info);
  if (getsockopt(Int_val(fd), IPPROTO_TCP, TCP_INFO, &info, &size) == 0)
    return Val_long(info.tcpi_rtt);
#elif defined(TCP_CONNECTION_INFO)
  struct tcp_connection_info info;
  socklen_t size = sizeof(info);
  if (getsockopt(Int_val(fd), IPPROTO_TCP, TCP_CONNECTION_INFO, &info, &size) == 0)
    return Val_long((long)info.tcpi_srtt * 1000);
#endif
  return Val_long(-1);
}

/* The most bytes the kernel lets one connection's send buffer grow to, or -1
   where it does not tell. */
value quiver_tcp_send_buffer_limit(value unit)
{
  long limit = -1;
#if defined(__linux__)
  long least, initial;
  FILE *settings = fopen("/proc/sys/net/ipv4/tcp_wmem", "r");
  if (settings != NULL) {
    if (fscanf(settings, "%ld %ld %ld", &least, &initial, &limit) != 3) limit = -1;
    fclose(settings);
  }
#elif defined(__APPLE__)
  int largest;
  size_t size = sizeof(largest);
  if (sysctlbyname("net.inet.tcp.autosndbufmax", &largest, &size, NULL, 0) == 0)
    limit = largest;
#endif
  return Val_long(limit);
}

/* Marks the connection's packets with this DSCP (0 to 63), for IPv4 and
   IPv6 alike; best effort. */
value quiver_tcp_mark(value fd, value dscp)
{
  int socket = Int_val(fd), tos = Int_val(dscp) << 2;
  quiver_set(socket, IPPROTO_IP, IP_TOS, tos);
#ifdef IPV6_TCLASS
  quiver_set(socket, IPPROTO_IPV6, IPV6_TCLASS, tos);
#endif
  return Val_unit;
}
