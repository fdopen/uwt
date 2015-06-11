module W = Uwt.C_worker
external c_glob: string -> string array option W.u -> W.t = "uwt_custom_test"
let glob pattern = W.call c_glob pattern
