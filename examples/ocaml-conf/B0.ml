open B0_kit.V000
open B00_std

let u = B0_unit.v "hey" @@ fun b k ->
  let m = B0_build.memo b in
  let comp = B00_ocaml.Tool.ocamlc in
  let bdir = B0_build.Unit.(build_dir b (current b)) in
  let conf = Fpath.(bdir / "ocaml.conf") in
  B00_ocaml.Conf.write m ~comp ~o:conf;
  B00_ocaml.Conf.read m conf @@ fun conf ->
  B00.Memo.notify m `Info
    "@[<v>%a@,%a@,%a@,%a@,%a@,%a@,%a@]"
    String.Map.pp_dump_string_map (B00_ocaml.Conf.to_string_map conf)
    Fmt.(field "where" B00_ocaml.Conf.where Fpath.pp_unquoted) conf
    Fmt.(field "asm_ext" B00_ocaml.Conf.asm_ext string) conf
    Fmt.(field "dll_ext" B00_ocaml.Conf.dll_ext string) conf
    Fmt.(field "exe_ext" B00_ocaml.Conf.exe_ext string) conf
    Fmt.(field "lib_ext" B00_ocaml.Conf.lib_ext string) conf
    Fmt.(field "obj_ext" B00_ocaml.Conf.obj_ext string) conf;
  k ()
