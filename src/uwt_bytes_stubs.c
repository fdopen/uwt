/* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix_stubs
 * Copyright (C) 2009-2010 Jérémie Dimino
 * Copyright (C) 2015 Andreas Hauptmann
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

#include "uwt_bytes_stubs.h"
/* +-----------------------------------------------------------------+
   | Operation on bigarrays                                          |
   +-----------------------------------------------------------------+ */

CAMLprim value uwt_unix_blit(value val_buf1, value val_ofs1, value val_buf2, value val_ofs2, value val_len)
{
  memmove((char*)Caml_ba_data_val(val_buf2) + Long_val(val_ofs2),
         (char*)Caml_ba_data_val(val_buf1) + Long_val(val_ofs1),
         Long_val(val_len));
  return Val_unit;
}

CAMLprim value uwt_unix_blit_from_bytes(value val_buf1, value val_ofs1, value val_buf2, value val_ofs2, value val_len)
{
  memcpy((char*)Caml_ba_data_val(val_buf2) + Long_val(val_ofs2),
         String_val(val_buf1) + Long_val(val_ofs1),
         Long_val(val_len));
  return Val_unit;
}

CAMLprim value uwt_unix_blit_to_bytes(value val_buf1, value val_ofs1, value val_buf2, value val_ofs2, value val_len)
{
  memcpy(String_val(val_buf2) + Long_val(val_ofs2),
         (char*)Caml_ba_data_val(val_buf1) + Long_val(val_ofs1),
         Long_val(val_len));
  return Val_unit;
}

CAMLprim value uwt_unix_fill_bytes(value val_buf, value val_ofs, value val_len, value val_char)
{
  memset((char*)Caml_ba_data_val(val_buf) + Long_val(val_ofs), Int_val(val_char), Long_val(val_len));
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
