#define CAML_NAME_SPACE
#ifdef __APPLE__
#include <sys/types.h>
#include <sys/sysctl.h>
#endif
#include <caml/mlvalues.h>

/* Physical performance cores on macOS (all physical cores on a machine with
   one kind), or -1 elsewhere; reads sysctls, touches no OCaml value. */
value quiver_cores_performance(value unit)
{
  (void)unit;
#ifdef __APPLE__
  int cores;
  size_t size = sizeof cores;
  if (sysctlbyname("hw.perflevel0.physicalcpu", &cores, &size, NULL, 0) == 0
      && cores > 0)
    return Val_int(cores);
  size = sizeof cores;
  if (sysctlbyname("hw.physicalcpu", &cores, &size, NULL, 0) == 0 && cores > 0)
    return Val_int(cores);
#endif
  return Val_int(-1);
}
