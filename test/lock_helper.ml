open Unix

let try_lock file =
  let fd =
    openfile
      file
      [O_WRONLY ; O_CREAT ]
      0644
  in
  try
    let () = lockf fd F_TLOCK 0 in
    exit 1
  with
  | Unix_error((EACCES|EAGAIN|EBUSY),"lockf",_) -> exit 0

let hold_lock file =
  let fd =
    openfile
      file
      [ O_WRONLY ; O_CREAT ]
      0644
  in
  let () = Unix.lockf fd Unix.F_TLOCK 0 in
  Unix.sleep 2;
  exit 0

let () =
  if Sys.argv.(1) = "-h" then
    hold_lock Sys.argv.(2)
  else
    try_lock Sys.argv.(1)

