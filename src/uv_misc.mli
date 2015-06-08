open Uv

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

val guess_handle: file -> handle_type

val resident_set_memory : unit -> nativeint result
val resident_set_memory_exn : unit -> nativeint

val uptime : unit -> float result
val uptime_exn : unit -> float

val getrusage : unit -> rusage result
val getrusage_exn : unit -> rusage

val cpu_info : unit -> cpu_info array result
val cpu_info_exn : unit -> cpu_info array

val interface_addresses: unit -> interface_address array result
val interface_addresses_exn: unit -> interface_address array

val load_avg: unit -> float * float * float

val ip4_addr: string -> int -> sockaddr result
val ip4_addr_exn: string -> int -> sockaddr

val ip4_name: sockaddr -> string result
val ip4_name_exn: sockaddr -> string

val ip6_addr: string -> int -> sockaddr result
val ip6_addr_exn: string -> int -> sockaddr

val ip6_name: sockaddr -> string result
val ip6_name_exn: sockaddr -> string

val get_total_memory: unit -> int64
val hrtime: unit -> int64

type version = {
  major: int;
  minor: int;
  patch: int;
}
val version: unit -> version
val version_raw: unit -> int
val version_string: unit -> string
val os_homedir: unit -> string result
