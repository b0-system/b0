
let () = match B0_descr.find_root (B0.Fpath.v Sys.argv.(1)) with
| Ok None -> print_endline "no root found"
| Ok (Some root) -> print_endline ("root: " ^ (B0.Fpath.to_string root))
| Error (`Msg m) -> prerr_endline m
