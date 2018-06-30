/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "config.h"
#include <string.h>
#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/alloc.h>
#include <caml/version.h>

#include "uwt_bytes_stubs.h"

#define Ba_buf_val(x) ((unsigned char*)Caml_ba_data_val(x))

/* +-----------------------------------------------------------------+
   | Operation on bigarrays                                          |
   +-----------------------------------------------------------------+ */

CAMLprim value uwt_unix_blit(value val_buf1, value val_ofs1, value val_buf2, value val_ofs2, value val_len)
{
  memmove(Ba_buf_val(val_buf2) + Long_val(val_ofs2),
          Ba_buf_val(val_buf1) + Long_val(val_ofs1),
          Long_val(val_len));
  return Val_unit;
}

CAMLprim value uwt_unix_blit_from_bytes(value val_buf1, value val_ofs1, value val_buf2, value val_ofs2, value val_len)
{
  memcpy(Ba_buf_val(val_buf2) + Long_val(val_ofs2),
         String_val(val_buf1) + Long_val(val_ofs1),
         Long_val(val_len));
  return Val_unit;
}

CAMLprim value uwt_unix_blit_to_bytes(value val_buf1, value val_ofs1, value val_buf2, value val_ofs2, value val_len)
{
  memcpy(String_val(val_buf2) + Long_val(val_ofs2),
         Ba_buf_val(val_buf1) + Long_val(val_ofs1),
         Long_val(val_len));
  return Val_unit;
}

CAMLprim value uwt_unix_fill_bytes(value val_buf, value val_ofs, value val_len, value val_char)
{
  memset(Ba_buf_val(val_buf) + Long_val(val_ofs), Int_val(val_char), Long_val(val_len));
  return Val_unit;
}

CAMLprim value uwt_unix_memchr(value val_buf, value val_ofs, value val_len, value val_char)
{
  const unsigned char * s = Caml_ba_data_val(val_buf);
  const intnat offset = Long_val(val_ofs);
  const intnat len = Long_val(val_len);
  const unsigned char needle = Int_val(val_char);
  unsigned char * p ;
  if ( len == 0 || (p=memchr(s+offset,needle,len)) == NULL ){
    return Val_long(-1);
  }
  else {
    intnat spos =  p - s;
    return Val_long(spos);
  }
}
