module U = Uwt
module M = Uv_misc

let show_sockaddr s =
  let open Uv in
  let open M in
  match ip4_name s with
  | Ok s -> s
  | Error _ ->
    match ip6_name s with
    | Ok s -> s
    | Error _ -> "(unknown)"

type sockaddr = Uv.sockaddr
module Uv = struct
  include Uv
  let pp_sockaddr fmt s =
    show_sockaddr s |> Format.fprintf fmt "%s"
end

type timeval = [%import: Uv_misc.timeval] [@@deriving show]
type rusage = [%import: Uv_misc.rusage] [@@deriving show]
type cpu_times = [%import: Uv_misc.cpu_times] [@@deriving show]
type cpu_info = [%import: Uv_misc.cpu_info] [@@deriving show]
type interface_address = [%import: Uv_misc.interface_address] [@@deriving show]

let show_sockaddr s =
  match M.ip4_name s with
  | Uv.Ok s -> s
  | Uv.Error _ ->
    match M.ip6_name s with
    | Uv.Ok s -> s
    | Uv.Error _ -> "(unknown)"

let () =
  M.hrtime () |> Printf.printf "Hrtime(start): %Ld\n";
  let t = M.getrusage_exn () in
  show_rusage t |> print_endline ;
  let ar = M.cpu_info_exn () in
  Array.iter ( fun s ->
      show_cpu_info s |> print_endline ) ar ;
  let ar = M.interface_addresses_exn () in
  Array.iter ( fun s ->
      show_interface_address s |> print_endline ;
    ) ar ;
  let (x,y,z) = M.load_avg () in
  Printf.printf "%f-%f-%f\n" x y z;
  M.get_total_memory () |> Printf.printf "Total Memory:%Ld\n";
  M.hrtime () |> Printf.printf "Hrtime(end): %Ld\n"
