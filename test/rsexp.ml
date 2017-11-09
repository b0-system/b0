

let () = match B0_sexp.of_file (B0.Fpath.v Sys.argv.(1)) with
| Ok l ->
(*    List.iter (B0_sexp.dump_locs Format.std_formatter) l *)
    begin match B0_sexp.list_to_string_map l with
    | Ok l -> ()
    | Error (`Msg m) -> prerr_endline m
    end
| Error (`Msg m) -> prerr_endline m
