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

      dep ["record_b0_stubs"] [lib "src-std/libb0_stubs"];

      flag_and_dep
        ["link"; "ocaml"; "link_b0_stubs"]
        (P (lib "src-std/libb0_stubs"));

      flag ["library"; "ocaml"; "byte"; "record_b0_stubs"]
        (S ([A "-dllib"; A "-lb0_stubs"] @ system_support_lib));

      flag ["library"; "ocaml"; (* byte and native *)
            "record_b0_stubs"]
        (S ([A "-cclib"; A "-lb0_stubs"] @ system_support_lib));

      ocaml_lib ~tag_name:"use_b0_stubs"
        ~dir:"src-std" "src-std/b0_std";

      flag ["link"; "ocaml"; "use_b0_stubs"]
        (S [A "-ccopt"; A "-Lsrc-std"]);
  | _ -> ()
  end
