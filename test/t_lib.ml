module W = Uwt.C_worker
external c_test: string * string -> bool W.u -> W.t = "uwt_external_test"
let string2file ~name ~content = W.call c_test (name,content)
