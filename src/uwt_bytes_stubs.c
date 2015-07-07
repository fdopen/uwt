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
#include "config.h"
#include <string.h>
#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/alloc.h>

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

CAMLprim value
uwt_unix_unsafe_setbuf(value val_buf, value val_ofs, value val)
{
  unsigned char * s = Caml_ba_data_val(val_buf);
  intnat i = Long_val(val_ofs);
  s[i] = Long_val(val);
  return Val_unit;
}

CAMLprim value
uwt_unix_unsafe_getbuf(value val_buf, value val_ofs)
{
  unsigned char * s = Caml_ba_data_val(val_buf);
  intnat i = Long_val(val_ofs);
  return (Val_long(s[i]));
}

#ifdef HAVE_SYS_BYTEORDER_H
#include <sys/byteorder.h>
#endif
#ifdef HAVE_SYS_ENDIAN_H
#include <sys/endian.h>
#endif

#ifdef HAVE_BUILTIN_BSWAP16
#define UBSWAP2 __builtin_bswap16
#endif
#ifdef HAVE_BUILTIN_BSWAP32
#define UBSWAP4 __builtin_bswap32
#endif
#ifdef HAVE_BUILTIN_BSWAP64
#define UBSWAP8 __builtin_bswap64
#endif

#if !defined(UBSWAP2) || !defined(UBSWAP4) || !defined(UBSWAP8)

#ifdef _MSC_VER
#ifndef UBSWAP2
#define UBSWAP2(x) _byteswap_ushort(x)
#endif
#ifndef UBSWAP4
#define UBSWAP4(x) _byteswap_ulong(x)
#endif
#ifndef UBSWAP8
#define UBSWAP8(x) _byteswap_uint64(x)
#endif

#elif defined(__APPLE__)
#include <libkern/OSByteOrder.h>
#ifndef UBSWAP2
#define UBSWAP2(x) OSSwapInt16(x)
#endif
#ifndef UBSWAP2
#define UBSWAP4(x) OSSwapInt32(x)
#endif
#ifndef UBSWAP8
#define UBSWAP8(x) OSSwapInt64(x)
#endif

#elif defined(HAVE_BYTESWAP_H)
#include <byteswap.h>
#ifndef UBSWAP2
#define UBSWAP2(x) bswap_16(x)
#endif
#ifndef UBSWAP4
#define UBSWAP4(x) bswap_32(x)
#endif
#ifndef UBSWAP8
#define UBSWAP8(x) bswap_64(x)
#endif

#elif defined(bswap32)
// FreeBSD defines bswap{16,32,64} in <sys/endian.h>
#ifndef UBSWAP2
#define UBSWAP2(x) bswap16(x)
#endif
#ifndef UBSWAP4
#define UBSWAP4(x) bswap32(x)
#endif
#ifndef UBSWAP8
#define UBSWAP8(x) bswap64(x)
#endif

#elif defined(BSWAP_64)
// Solaris 10 defines BSWAP_{16,32,64} in <sys/byteorder.h>
#ifndef UBSWAP2
#define UBSWAP2(x) UBSWAP_16(x)
#endif
#ifndef UBSWAP4
#define UBSWAP4(x) UBSWAP_32(x)
#endif
#ifndef UBSWAP8
#define UBSWAP8(x) UBSWAP_64(x)
#endif

#else
#ifndef UBSWAP2
static FORCE_INLINE uint16_t UBSWAP2(uint16_t x){
  return (x << 8) | (x >> 8);
}
#endif
#ifndef UBSWAP4
static FORCE_INLINE uint32_t UBSWAP4(uint32_t x){
  x = ((x & 0xff00ff00UL) >> 8) | ((x & 0x00ff00ffUL) << 8);
  return (x >> 16) | (x << 16);
}
#endif
#ifndef UBSWAP8
static FORCE_INLINE uint64_t UBSWAP8(uint64_t x){
  x = ((x & 0xff00ff00ff00ff00ULL) >> 8) | ((x & 0x00ff00ff00ff00ffULL) << 8);
  x = ((x & 0xffff0000ffff0000ULL) >> 16) | ((x & 0x0000ffff0000ffffULL) << 16);
  return (x >> 32) | (x << 32);
}
#endif

#endif /* else */
#endif /* !defined(UBSWAP2) || !defined(UBSWAP4) || !defined(UBSWAP8) */

#ifdef ENDIAN_BIG
#define SWAP_CODE 1
#else
#define SWAP_CODE 0
#endif

CAMLprim value
uwt_unix_read_int(value val_buf, value val_ofs, value bo)
{
  const unsigned char * s = Caml_ba_data_val(val_buf);
  uint32_t n = *((uint32_t*)(s + Long_val(val_ofs)));
  if ( Int_val(bo) == SWAP_CODE ){
    n = UBSWAP4(n);
  }
  return (Val_long((int32_t)n));
}

CAMLprim value
uwt_unix_read_int16(value val_buf, value val_ofs, value bo)
{
  const unsigned char * s = Caml_ba_data_val(val_buf);
  uint16_t n = *((uint16_t*)(s + Long_val(val_ofs)));
  if ( Int_val(bo) == SWAP_CODE ){
    n = UBSWAP2(n);
  }
  return (Val_long((int16_t)n));
}

CAMLprim value
uwt_unix_read_int32(value val_buf, value val_ofs, value bo)
{
  const unsigned char * s = Caml_ba_data_val(val_buf);
  uint32_t n = *((uint32_t*)(s + Long_val(val_ofs)));
  if ( Int_val(bo) == SWAP_CODE ){
    n = UBSWAP4(n);
  }
  return (caml_copy_int32((int32_t)n));
}

CAMLprim value
uwt_unix_read_int64(value val_buf, value val_ofs, value bo)
{
  const unsigned char * s = Caml_ba_data_val(val_buf);
  uint64_t n = *((uint64_t*)(s + Long_val(val_ofs)));
  if ( Int_val(bo) == SWAP_CODE ){
    n = UBSWAP8(n);
  }
  return (caml_copy_int64((int64_t)n));
}

CAMLprim value
uwt_unix_read_float32(value val_buf, value val_ofs, value bo)
{
  union { float f; uint32_t ui; } u;
  const unsigned char * s = Caml_ba_data_val(val_buf);
  uint32_t n = *((uint32_t*)(s + Long_val(val_ofs)));
  if ( Int_val(bo) == SWAP_CODE ){
    n = UBSWAP4(n);
  }
  u.ui = (int32_t)n;
  return (caml_copy_double(u.f));
}

CAMLprim value
uwt_unix_read_float64(value val_buf, value val_ofs, value bo)
{
  union { double d; uint64_t ui; uint32_t h[2]; } u;
  const unsigned char * s = Caml_ba_data_val(val_buf);
  uint64_t n = *((uint64_t*)(s + Long_val(val_ofs)));
  if ( Int_val(bo) == SWAP_CODE ){
    n = UBSWAP8(n);
  }
  u.ui = n;
#if defined(__arm__) && !defined(__ARM_EABI__)
  uint32_t t = u.h[0]; u.h[0] = u.h[1]; u.h[1] = t;
#endif
  return (caml_copy_double(u.d));
}

CAMLprim value
uwt_unix_write_int(value val_buf, value val_ofs, value val, value bo)
{
  unsigned char * s = Caml_ba_data_val(val_buf);
  uint32_t * p = (uint32_t*)(s + Long_val(val_ofs));
  uint32_t n = (uint32_t)(Long_val(val));
  if ( Int_val(bo) == SWAP_CODE ){
    n = UBSWAP4(n);
  }
  *p = n;
  return Val_unit;
}

CAMLprim value
uwt_unix_write_int16(value val_buf, value val_ofs, value val, value bo)
{
  unsigned char * s = Caml_ba_data_val(val_buf);
  uint16_t * p = (uint16_t*)(s + Long_val(val_ofs));
  uint16_t n = (uint16_t)(Long_val(val));
  if ( Int_val(bo) == SWAP_CODE ){
    n = UBSWAP2(n);
  }
  *p = n;
  return Val_unit;
}

CAMLprim value
uwt_unix_write_int32(value val_buf, value val_ofs, value val, value bo)
{
  unsigned char * s = Caml_ba_data_val(val_buf);
  uint32_t * p = (uint32_t*)(s + Long_val(val_ofs));
  uint32_t n = (uint32_t)(Int32_val(val));
  if ( Int_val(bo) == SWAP_CODE ){
    n = UBSWAP4(n);
  }
  *p = n;
  return Val_unit;
}

CAMLprim value
uwt_unix_write_int64(value val_buf, value val_ofs, value val, value bo)
{
  unsigned char * s = Caml_ba_data_val(val_buf);
  uint64_t * p = (uint64_t*)(s + Long_val(val_ofs));
  uint64_t n = (uint64_t)(Int64_val(val));
  if ( Int_val(bo) == SWAP_CODE ){
    n = UBSWAP8(n);
  }
  *p = n;
  return Val_unit;
}

CAMLprim value
uwt_unix_write_float32(value val_buf, value val_ofs, value val, value bo)
{
  union { float f; uint32_t ui; } u;
  unsigned char * s = Caml_ba_data_val(val_buf);
  uint32_t * p = (uint32_t*)(s + Long_val(val_ofs));
  u.f = Double_val(val);
  if ( Int_val(bo) == SWAP_CODE ){
    u.ui = UBSWAP4(u.ui);
  }
  memcpy(p,&u.ui,sizeof *p);
  return Val_unit;
}

CAMLprim value
uwt_unix_write_float64(value val_buf, value val_ofs, value val, value bo)
{
  union { double d; uint64_t ui; uint32_t h[2]; } u;
  unsigned char * s = Caml_ba_data_val(val_buf);
  uint64_t * p = (uint64_t*)(s + Long_val(val_ofs));
  u.d = Double_val(val);
#if defined(__arm__) && !defined(__ARM_EABI__)
  uint32 t = u.h[0]; u.h[0] = u.h[1]; u.h[1] = t;
#endif
  if ( Int_val(bo) == SWAP_CODE ){
    u.ui = UBSWAP8(u.ui);
  }
  memcpy(p,&u.ui,sizeof *p);
  return Val_unit;
}
