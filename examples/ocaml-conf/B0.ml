open B0_kit.V000
open B0_std

let u = B0_unit.v "hey" @@ fun b k ->
  let m = B0_build.memo b in
  let comp = B0_ocaml.Tool.ocamlc in
  let bdir = B0_unit.(build_dir b (current b)) in
  let conf = Fpath.(bdir / "ocaml.conf") in
  B0_ocaml.Conf.write m ~comp ~o:conf;
  B0_ocaml.Conf.read m conf @@ fun conf ->
  B0_memo.Memo.notify m `Info
    "@[<v>%a@,%a@,%a@,%a@,%a@,%a@,%a@]"
    String.Map.pp_dump_string_map (B0_ocaml.Conf.to_string_map conf)
    Fmt.(field "where" B0_ocaml.Conf.where Fpath.pp_unquoted) conf
    Fmt.(field "asm_ext" B0_ocaml.Conf.asm_ext string) conf
    Fmt.(field "dll_ext" B0_ocaml.Conf.dll_ext string) conf
    Fmt.(field "exe_ext" B0_ocaml.Conf.exe_ext string) conf
    Fmt.(field "lib_ext" B0_ocaml.Conf.lib_ext string) conf
    Fmt.(field "obj_ext" B0_ocaml.Conf.obj_ext string) conf;
  k ()
