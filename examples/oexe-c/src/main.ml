
external q : unit -> int = "ocaml_question"

let () = Printf.printf "The answer is: %d\n" (q ())
