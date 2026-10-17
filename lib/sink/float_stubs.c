#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include "zmij-c.h"

/* The function does not allocate, so the OCaml block behind [bytes] cannot move
   while zmij writes into it; the caller reserves zmij's buffer size first. */

intnat quiver_write_double(value bytes, intnat offset, double number)
{
  char *start = (char *)Bytes_val(bytes) + offset;
  return zmij_detail_write(number, start) - start;
}

value quiver_write_double_byte(value bytes, value offset, value number)
{
  return Val_long(
      quiver_write_double(bytes, Long_val(offset), Double_val(number)));
}
