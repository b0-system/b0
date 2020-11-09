
let main () =
  let tid = Thread.create (fun () -> print_endline "Ha!") () in
  Thread.join tid;
  print_endline "Ho!"

let () = main ()
