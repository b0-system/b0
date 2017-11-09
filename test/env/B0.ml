
(* Test Build.spawn environment handling *)

open B0

let home = "HOME"

(* FIXME this should force Build_aim lookup *)
let env_bin b = match Build.conf b B0_care.OS.name with
| "win32" -> "SET"
| _ -> "env"

let env_tool env_bin = Tool.v env_bin ~internal:[]
let env_tool_home env_bin = Tool.v env_bin ~internal:[] ~env_vars:[home]

let parse_env s = OS.Env.of_assignments @@ String.cuts ~empty:false ~sep:"\n" s

let extract_forced_env env =
  let force = "B0_FORCE_" in
  let add var v acc = match var with
  | var when String.is_prefix ~affix:force var ->
      let var = String.with_index_range ~first:(String.length force) var in
      String.Map.add var v acc
  | _ -> acc
  in
  String.Map.fold add env String.Map.empty

let assert_env b n env_tool env ~wit =
  let assert_env wit env =
    try
      let assert_binding var w v = match w, v with
      | None, Some v ->
          failwith (strf "Unexpected variable in spawn: %s=%s" var v)
      | Some v, None ->
          failwith (strf "Missing variable in spawn: %s=%s" var v)
      | Some v0, Some v1 when not (String.equal v0 v1) ->
          failwith
            (strf "Variable %s in spawn, expected %S, found %S" var v0 v1)
      | Some _, Some _ -> None
      | None, None -> assert false
      in
      ignore (String.Map.merge assert_binding wit env);
      ()
    with
    | Failure e -> Build.fail (fun m -> m "%s" e)
  in
  let out = Build.build_file b (strf "env.%d" n) in
  Build.spawn b ~env ~writes:[out] ~stdout:(`File out) ~exits:[] @@
  env_tool Cmd.empty;
  Build.read b out
    (fun d -> assert_env wit (Build.fail_on_error_msg (parse_env d)))

let empty =
  let build b =
    let env_bin = env_bin b in
    let env_tool = Build.tool b (env_tool env_bin) in
    let env_tool_home = Build.tool b (env_tool_home env_bin) in
    let build_env = Env.env (Build.env b) `Host_os in
    let forced_env = Env.forced_env (Build.env b) `Host_os in
    let home_env = String.Map.(add home "/dev/null" empty) in
    let forced_and_maybe_home = match String.Map.find home forced_env with
    | v -> forced_env
    | exception Not_found ->
        match String.Map.find home build_env with
        | v -> String.Map.add home v forced_env
        | exception Not_found -> forced_env
    in
    let forced_and_home = match String.Map.find home forced_env with
    | v -> forced_env
    | exception Not_found ->
        String.Map.add home (String.Map.find home home_env) forced_env
    in
    assert_env b 0 env_tool String.Map.empty ~wit:forced_env;
    assert_env b 1 env_tool_home String.Map.empty ~wit:forced_and_maybe_home;
    assert_env b 2 env_tool home_env ~wit:forced_and_home;
    assert_env b 3 env_tool_home home_env ~wit:forced_and_home;
  in
  Unit.create "env" build ~doc:"Test spawn environements"
