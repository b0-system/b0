open B0

(* Test Hash.{string,file} *)

let build b =
  let assert_file_hash b f h =
    Build.read b f (fun data -> assert (Hash.(equal (string data) h)))
  in
  let zero = Build.src b Fpath.(v "zero") in
  let hashme = Build.src b Fpath.(v "hashme") in
  assert_file_hash b zero (Build.fail_on_error_msg @@ Hash.file zero);
  assert_file_hash b hashme (Build.fail_on_error_msg @@ Hash.file hashme);
  ()

let hash =
  Unit.create "hash" build ~doc:"Testing hashing."
