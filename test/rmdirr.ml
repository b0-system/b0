
let () =
  match B0.OS.Dir.delete ~contents:true (Fpath.v Sys.argv.(1)) with
  | Ok () -> ()
  | Error (`Msg m) -> prerr_endline m
