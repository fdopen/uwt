(* boilerplate for output *)
let show_sockaddr s =
  let open Uwt in
  let open Misc in
  match ip4_name s with
  | Ok ("0.0.0.0" as x) ->
    (match ip6_name s with
    | Ok s -> s
    | Error _ -> x)
  | Ok s -> s
  | Error _ ->
    match ip6_name s with
    | Ok s -> s
    | Error _ -> "(unknown)"

let pp_sockaddr fmt s = show_sockaddr s |> Format.fprintf fmt "%s"

type socket_domain = [%import: Unix.socket_domain] [@@deriving show]
type socket_type = [%import: Unix.socket_type] [@@deriving show]
type getaddrinfo_option = [%import: Unix.getaddrinfo_option] [@@deriving show]
type service_entry = [%import: Unix.service_entry] [@@deriving show]

type sockaddr = [%import: Uwt_base.sockaddr]

type file_kind = [%import: Uwt_base.Fs_types.file_kind] [@@deriving show]
type stats = [%import: Uwt_base.Fs_types.stats] [@@deriving show]

type timeval = [%import: Uwt_base.Misc.timeval] [@@deriving show]
type rusage = [%import: Uwt_base.Misc.rusage] [@@deriving show]
type cpu_times = [%import: Uwt_base.Misc.cpu_times] [@@deriving show]
type cpu_info = [%import: Uwt_base.Misc.cpu_info] [@@deriving show]
type interface_address = [%import: Uwt_base.Misc.interface_address] [@@deriving show]

type addr_info = [%import: Uwt.Dns.addr_info] [@@deriving show]
type name_info = [%import: Unix.name_info] [@@deriving show]

type win_version = [%import: Uwt_base.Sys_info.win_version] [@@deriving show]

module Fs_event = struct
  type event = [%import: Uwt.Fs_event.event] [@@deriving show]
  type cb_res = string * event list [@@deriving show]
end

module Udp = struct
  type error = [%import: Uwt_base.error] [@@deriving show]
  type recv_result = Uwt.Udp.recv_result =
    | Data of Bytes.t * sockaddr option
    | Partial_data of Bytes.t * sockaddr option
    | Empty_from of sockaddr
    | Transmission_error of error [@@deriving show]
end
