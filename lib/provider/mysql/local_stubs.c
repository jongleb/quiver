#include <caml/mlvalues.h>
#ifdef __APPLE__
#include <sys/types.h>
#include <sys/sysctl.h>
#endif

/* Reads one integer sysctl; no OCaml values are allocated or kept. */
value quiver_local_send_space(value unit)
{
  (void)unit;
#ifdef __APPLE__
  int space;
  size_t size = sizeof space;
  if (sysctlbyname("net.local.stream.sendspace", &space, &size, NULL, 0) == 0)
    return Val_int(space);
#endif
  return Val_int(-1);
}
