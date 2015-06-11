module U = Uwt
module M = Uwt.Misc

open Show_uwt

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
