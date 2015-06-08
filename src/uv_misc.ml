open Uv

let to_exn n = function
| Ok x -> x
| Error x -> raise (Uv_error(x,n,""))

type timeval = {
  sec: int; usec: int;
}

type rusage = {
  utime: timeval;
  stime: timeval;
  maxrss: int64;
  ixrss: int64;
  idrss: int64;
  isrss: int64;
  minflt: int64;
  majflt: int64;
  nswap: int64;
  inblock: int64;
  outblock: int64;
  msgsnd: int64;
  msgrcv: int64;
  nsignals: int64;
  nvcsw: int64;
  nivcsw: int64;
}

type cpu_times = {
  user: int64;
  nice: int64;
  sys: int64;
  idle: int64;
  irq: int64;
}

type cpu_info = {
  model: string;
  speed: int;
  cpu_times: cpu_times;
}

type interface_address = {
  name: string;
  phys_addr: string;
  is_internal: bool;
  address: sockaddr;
  netmask: sockaddr;
}

type handle_type =
  | File
  | Tty
  | Pipe
  | Tcp
  | Udp
  | Unknown

external guess_handle:
  file -> handle_type = "uwt_guess_handle_na" "noalloc"

external resident_set_memory:
  unit -> nativeint result = "uwt_resident_set_memory"
let resident_set_memory_exn () =
  resident_set_memory () |> to_exn "uv_resident_set_memory"

external uptime: unit -> float result = "uwt_uptime"
let uptime_exn () = uptime () |> to_exn "uv_uptime"

external getrusage : unit -> rusage result = "uwt_getrusage"
let getrusage_exn () = getrusage () |> to_exn "uv_getrusage"

external cpu_info: unit -> cpu_info array result = "uwt_cpu_info"
let cpu_info_exn () = cpu_info () |> to_exn "uv_cpu_info"

external interface_addresses:
  unit -> interface_address array result = "uwt_interface_addresses"
let interface_addresses_exn () =
  interface_addresses () |> to_exn "uv_interface_addresses"

external load_avg: unit -> float * float * float = "uwt_load_avg"

external ip4_addr: string -> int -> sockaddr result = "uwt_ip4_addr"
let ip4_addr_exn s i = ip4_addr s i |> to_exn "uv_ip4_addr"
external ip4_name: sockaddr -> string result = "uwt_ip4_name"
let ip4_name_exn s = ip4_name s |> to_exn "uv_ip4_name"

external ip6_addr: string -> int -> sockaddr result = "uwt_ip6_addr"
let ip6_addr_exn s i = ip6_addr s i |> to_exn "uv_ip6_addr"
external ip6_name: sockaddr -> string result = "uwt_ip6_name"
let ip6_name_exn s = ip6_name s |> to_exn "uv_ip6_name"

external get_total_memory: unit -> int64 = "uwt_get_total_memory"
external hrtime: unit -> int64 = "uwt_hrtime"

type version = {
  major: int;
  minor: int;
  patch: int;
}

external version_raw: unit -> int = "uwt_version_na" "noalloc"
let version () =
  let n = version_raw () in
  {
    patch = n land 0xff;
    minor = (n lsr 8 ) land 0xff;
    major = (n lsr 16) land 0xff
  }

external version_string: unit -> string = "uwt_version_string"
external os_homedir: unit -> string result = "uwt_os_homedir"
