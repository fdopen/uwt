(** synchronous system calls *)

(**
   These synchronous might be useful, if you also target windows users:
   - filenames must be utf8 encoded (see comment to ECHARSET)
   - sometimes additional options are available, e.g. [Fs.openfile]
*)

include Uwt_base.Fs_functions
  with type 'a t := 'a Uwt_base.result
