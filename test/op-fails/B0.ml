
open B0

let unreachable _ = assert false

(* FIXME would be nice to port to windows. *)

let echo b = Cmd.(v "echo")
let ls b = Cmd.(v "ls")

(* Testing expected and unexpected failures *)

let fail =
  let build b = Build.fail (fun m -> m "Wow I'm failing badly here") in
  Unit.create "fail" build ~doc:"Testing Build.fail"

let exn_fail =
  let build b = raise (Invalid_argument "Programming error my friend") in
  Unit.create "exn_fail" build ~doc:"Testing unit unexpectedly raising."

let never =
  let build b = Build.read b (Build.build_file b "never") unreachable in
  Unit.create "never" build ~doc:"Testing reading a file that doesn't exist."

(* Testing operation failures *)

let ls_never ?success b =
  Build.spawn b ?success @@ Cmd.(ls b % p (Build.build_file b "never"))

let nz_exit_fail0 =
  let build = ls_never ?success:None in
  Unit.create "nz_exit_fail0" build ~doc:"Testing non zero exit fail."

let nz_exit_fail1 =
  let build = ls_never ~success:[2] in
  Unit.create "nz_exit_fail1" build ~doc:"Testing non zero exit fail."

let nz_exit_fail2 =
  let build = ls_never ~success:[0;2] in
  Unit.create "nz_exit_fail2" build ~doc:"Testing non zero exit fail."

let nz_exit_success0 =
  let build = ls_never ~success:[1] in
  Unit.create "nz_exit_success0" build ~doc:"Testing non zero exit success"

let nz_exit_success1 =
  let build = ls_never ~success:[] in
  Unit.create "nz_exit_success1" build ~doc:"Testing non zero exit success"

(* Testing operation deadlocks *)

let never_rw b r w =
  Build.spawn b ~reads:[r] ~writes:[w] @@ Cmd.(echo b % "-n")

let op_self_cycle =
  let build b =
    let never = Build.build_file b "never" in
    never_rw b never never
  in
  Unit.create "op_self_cycle" build ~doc:"Testing operation awaiting on itself."

let op_cycle_1 =
  let build b =
    let x, y = Build.(build_file b "x", build_file b "y") in
    never_rw b x y;
    never_rw b y x;
  in
  Unit.create "op_cycle_1" build ~doc:"Testing cyclic operations (1 step)"

let op_cycle_1 =
  let build b =
    let x, y, z = Build.(build_file b "x", build_file b "y", build_file b "z")in
    never_rw b x y;
    never_rw b y z;
    never_rw b z y;
  in
  Unit.create "op_cycle_2" build ~doc:"Testing cyclic operations (2 step)"

(* Testing unit deadlocks *)

let rec unit_self_cycle =
  let build b = Build.await_units b [Build.unit b] unreachable in
  Unit.create "unit_self_cycle" build ~doc:"Testing unit awaiting on itself."

let await uname b =
  let has_basename b u = String.equal (Unit.basename u) b in
  let uc11 = List.filter (has_basename uname) (Build.units b) in
  Build.await_units b uc11 unreachable

let doc = "Testing unit await deadlock (1 step)"
let uc10 = Unit.create "uc10" (await "uc11") ~doc
let uc11 = Unit.create "uc11" (await "uc10") ~doc

let doc = "Testing unit await deadlock (2 step)"
let uc20 = Unit.create "uc20" (await "uc21") ~doc
let uc21 = Unit.create "uc21" (await "uc22") ~doc
let uc22 = Unit.create "uc22" (await "uc20") ~doc
