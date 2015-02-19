module U = Uwt
module M = U.Misc

let show_sockaddr s =
  let open U in
  let open M in
  match ip4_name s with
  | Ok s -> s
  | Error _ ->
    match ip6_name s with
    | Ok s -> s
    | Error _ -> "(unknown)"

let pp_sockaddr fmt s =
  show_sockaddr s |> Format.fprintf fmt "%s"

type timeval = [%import: Uwt.Misc.timeval] [@@deriving show]
type rusage = [%import: Uwt.Misc.rusage] [@@deriving show]
type cpu_times = [%import: Uwt.Misc.cpu_times] [@@deriving show]
type cpu_info = [%import: Uwt.Misc.cpu_info] [@@deriving show]
type sockaddr = Uwt.sockaddr
type interface_address = [%import: Uwt.Misc.interface_address] [@@deriving show]

let show_sockaddr s =
  match M.ip4_name s with
  | U.Ok s -> s
  | U.Error _ ->
    match M.ip6_name s with
    | U.Ok s -> s
    | U.Error _ -> "(unknown)"

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
