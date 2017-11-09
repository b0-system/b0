open B0

(* Shows how units can wait on each other.

   In general do not try to wait on all units, this may lead to
   deadlocks. Use build unit metadata to select the ones to wait on. *)

let last b =
  let not_self u = not (Unit.equal u (Build.unit b)) in
  let units = List.filter not_self (Build.units b) in
  Build.await_units b units begin fun () ->
    Log.app (fun m -> m "Last unit in town !")
  end

let last =
  Unit.create "last" last ~doc:"Last unit in town (don't do this)"
