#include <caml/version.h>

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

CAMLprim value
uwt_unix_unsafe_setbuf(value,value,value);

CAMLprim value
uwt_unix_unsafe_getbuf(value,value);

CAMLprim value
uwt_unix_read_int(value,value,value);

CAMLprim value
uwt_unix_read_int16(value,value,value);

CAMLprim value
uwt_unix_read_int32(value,value,value);

CAMLprim value
uwt_unix_read_int64(value,value,value);

CAMLprim value
uwt_unix_read_float32(value,value,value);

CAMLprim value
uwt_unix_read_float64(value,value,value);

CAMLprim value
uwt_unix_write_int(value,value,value,value);

CAMLprim value
uwt_unix_write_int16(value,value,value,value);

CAMLprim value
uwt_unix_write_int32(value,value,value,value);

CAMLprim value
uwt_unix_write_int64(value,value,value,value);

CAMLprim value
uwt_unix_write_float32(value,value,value,value);

CAMLprim value
uwt_unix_write_float64(value,value,value,value);

#if OCAML_VERSION_MAJOR > 4 || (OCAML_VERSION_MAJOR == 4 && OCAML_VERSION_MINOR >= 3)
CAMLprim int32_t
uwt_unix_read_int32_native(value,value,value);

CAMLprim int64_t
uwt_unix_read_int64_native(value,value,value);

CAMLprim double
uwt_unix_read_float32_native(value,value,value);

CAMLprim double
uwt_unix_read_float64_native(value,value,value);

CAMLprim value
uwt_unix_write_int32_native(value, value, uint32_t, value);

CAMLprim value
uwt_unix_write_int64_native(value, value, uint64_t, value);

CAMLprim value
uwt_unix_write_float32_native(value, value, double d,value);

CAMLprim value
uwt_unix_write_float64_native(value, value, double d, value);
#endif
