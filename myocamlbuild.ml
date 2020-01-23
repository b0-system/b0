open Ocamlbuild_plugin
open Command

let os = Ocamlbuild_pack.My_unix.run_and_read "uname -s"

let lib s = match !Ocamlbuild_plugin.Options.ext_lib with
 | "" -> s ^ ".a"
 | x -> s ^ "." ^ x

let system_support_lib = match os with
| "Linux\n" -> [A "-cclib"; A "-lrt"]
| _ -> []

let () =
  dispatch begin function
  | After_rules ->

      dep ["compile";"c"]
          ["src-b00-std/b00_stubs.h";
           "src-b00-std/vendor/xxhash.h";
           "src-b00-std/vendor/MurmurHash3.h"];

      dep ["record_b00_stubs"] [lib "src-b00-std/libb00_stubs"];

      flag_and_dep
        ["link"; "ocaml"; "link_b00_stubs"]
        (P (lib "src-b00-std/libb00_stubs"));

      flag ["library"; "ocaml"; "byte"; "record_b00_stubs"]
        (S ([A "-dllib"; A "-lb00_stubs"] @ system_support_lib));

      flag ["library"; "ocaml"; (* byte and native *)
            "record_b00_stubs"]
        (S ([A "-cclib"; A "-lb00_stubs"] @ system_support_lib));

      ocaml_lib ~tag_name:"use_b00_stubs"
        ~dir:"src-b00-std" "src-b00-std/b00_std";

      flag ["link"; "ocaml"; "use_b00_stubs"]
        (S [A "-ccopt"; A "-Lsrc-b00-std"]);
  | _ -> ()
  end
