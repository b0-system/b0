
let hello () = print_endline "Hello World!"

open Cmdliner

let () =
  let hello_t = Term.(const hello $ const ()) in
  Term.exit @@ Term.eval (hello_t, Term.info "hello")
