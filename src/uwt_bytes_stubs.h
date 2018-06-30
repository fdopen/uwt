/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

CAMLextern value
uwt_unix_blit(value,value,value,value,value);

CAMLprim value
uwt_unix_blit_from_bytes(value,value,value,value,value);

CAMLprim value
uwt_unix_blit_to_bytes(value,value,value,value,value);

CAMLprim value
uwt_unix_fill_bytes(value,value,value,value);

CAMLprim value
uwt_unix_memchr(value,value,value,value);
