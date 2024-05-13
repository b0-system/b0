(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () = B0_scope.open_lib ~module':__MODULE__ "show-url"

open B0_std
open Result.Syntax

let url_of_path _env path =
  let* exists = Os.Path.exists path in (* FIXME remote build the build env *)
  if not exists
  then Fmt.error "%a: Not such path" Fpath.pp path
  else Ok (Fmt.str "file://%s" (Fpath.to_string path))

(* URLs *)

type url =
[ `Url of Url.t
| `In of B0_env.dir * Fpath.t
| `Fun of string * (B0_env.t -> B0_unit.t -> (Url.t, string) result) ]

let pp_url ppf = function
| `Url u -> Fmt.pf ppf "URL %s" u
| `In (dir, p) -> Fmt.pf ppf "%a in %a" Fpath.pp p B0_env.pp_dir dir
| `Fun (doc, _) -> Fmt.pf ppf "<fun> %s" doc

let url : url B0_meta.key =
  let doc = "The default URL to show." in
  let default = `In (`Unit_dir, Fpath.v ".") in
  B0_meta.Key.make "url" ~default ~doc ~pp_value:pp_url

let get_url env unit = match B0_unit.find_or_default_meta url unit with
| `Url url -> Ok url
| `In (`Unit_dir, p) ->
    let dir = B0_env.unit_dir env unit in
    let p = if Fpath.is_current_dir p then dir else Fpath.(dir // p) in
    url_of_path env (B0_env.in_unit_dir env unit p)
| `In (dir, p) -> url_of_path env (B0_env.in_dir env dir p)
| `Fun (_, f) -> f env unit

(* Server mode *)

let listen_args =
  let doc = "The command line arguments used to listen on an authority." in
  let default = fun ~authority -> Cmd.(arg "--listen" % authority) in
  let pp_value ppf _ = Fmt.pf ppf "<fun>, default is --listen AUTHORITY" in
  B0_meta.Key.make "listen-args" ~doc ~default ~pp_value:pp_value

let timeout_s =
  let doc =
    "Maximal number of seconds to wait for the server to be connectable \
     before showing the URL."
  in
  B0_meta.Key.make "timeout-s" ~doc ~default:1 ~pp_value:Fmt.int

let make_server_cmd cmd args ~listen_args =
  (* Inserts ~listen_args at the end of args or before -- *)
  let prev, rest =
    let rec loop prev = function
    | "--" :: rest as l -> prev, l
    | [] -> prev, []
    | arg :: args -> loop (arg :: prev) args
    in
    loop [] args
  in
  let args = List.rev_append prev (Cmd.to_list listen_args) @ rest in
  Cmd.(cmd %% of_list Fun.id args)

let endpoint_of_url_and_authority url authority =
  match Url.scheme url with
  | None -> Fmt.error "URL %s: no scheme found" url
  | Some "http" -> Os.Socket.Endpoint.of_string ~default_port:80 authority
  | Some "https" -> Os.Socket.Endpoint.of_string ~default_port:443 authority
  | Some scheme ->
      (Log.warn @@ fun m ->
       m "Unknown scheme %a using 80 as the default port" Fmt.code scheme);
      Os.Socket.Endpoint.of_string ~default_port:80 authority

let find_server_mode_unit = function
| [] -> Ok None
| entity :: args ->
    let keep = B0_unit.tool_is_user_accessible in
    match B0_unit.get_or_suggest_tool ~keep entity with
    | Ok us ->
        (* FIXME let u = check_tool_ambiguities name us in *)
        Ok (Some (List.hd us, args))
    | Error tool_suggs ->
        let u = B0_unit.get_or_suggest entity in
        match u with
        | Ok u -> Ok (Some (u, args))
        | Error us ->
            let tname u = Option.get (B0_unit.find_meta B0_unit.tool_name u) in
            let ts = List.rev_map tname tool_suggs in
            let us = List.rev_map B0_unit.name us in
            let set = String.Set.of_list (List.concat [ts; us]) in
            let suggs = String.Set.elements set in
            let hint = Fmt.did_you_mean in
            let nothing_to ppf v =
              Fmt.pf ppf "Nothing to execute for %a." Fmt.code v
            in
            let pp ppf (v, hints) = match hints with
            | [] -> nothing_to ppf v
            | hints -> Fmt.pf ppf "%a@ %a" nothing_to v (hint Fmt.code) hints
            in
            Fmt.error "@[%a@]" pp (entity, suggs)

let server_mode env timeout_cli no_exec ~url args = match Url.authority url with
| None -> Fmt.error "Could not extract authority from %s" url
| Some authority ->
    let* endpoint = endpoint_of_url_and_authority url authority in
    let* unit = find_server_mode_unit args in
    match unit with
    | None ->
        if not no_exec then
          (Log.warn @@ fun m ->
           m "@[<v>No tool specified but trying to connect to %s@,\
              Use option %a to suppress this warning.@]"
             authority Fmt.code "--no-exec");
        let timeout = match timeout_cli with
        | Some timeout -> timeout
        | None -> B0_meta.Key.get_default timeout_s
        in
        Ok (`Show_url_no_exec (endpoint, timeout, url))
    | Some (unit, args) ->
        (* XXX at somepoint we should be able to invoke the same
             logic as [B0_cmd_build.find_store_and_execution]. *)
        (* XXX We might actually want to look into actions aswell.
             See the todo.mld for more details.
             FIXME This is quite messy we need to clarify executable
             units ideally we should be using B0_unit.find_exec *)
        (* FIXME this should execute according to the build unit execution
             protocol. *)
        let* cmd = Result.map Cmd.path (B0_env.unit_exe_file env unit) in
        let timeout = match timeout_cli with
        | Some timeout -> timeout
        | None -> B0_unit.find_or_default_meta timeout_s unit
        in
        let listen_args =
          (B0_unit.find_or_default_meta listen_args unit) ~authority
        in
        let cmd = make_server_cmd cmd args ~listen_args in
        let* cwd = B0_unit.Exec.get_cwd env unit in
        let* env' = B0_unit.Exec.get_env env unit in
        let env' = Os.Env.to_assignments env' in
        Ok (`Show_url_server (endpoint, timeout, url, cmd, cwd, env'))

(* Unit .show-url.url mode *)

let parse_unit_specs args =
  let parse_arg arg = match String.cut_left ~sep:":" arg with
  | None -> arg, (arg, None)
  | Some (uname, path) -> uname, (arg, Some path)
  in
  List.map parse_arg args

let unit_mode env args =
  let specs = parse_unit_specs args in
  let unit_names, paths = List.split specs in
  let* units = B0_unit.get_list_or_hint ~all_if_empty:false unit_names in
  let specs = List.combine units paths in
  let make_url (unit, (arg, p)) =
    Result.error_to_failure @@
    Result.map_error (fun e -> Fmt.str "%a: %s" Fmt.code arg e) @@
    let unit_dir = B0_build.unit_dir (B0_env.build env) unit in
    match p with
    | Some p -> url_of_path env Fpath.(unit_dir // v p)
    | None -> get_url env unit
  in
  try Ok (`Show_unit_urls (List.map make_url specs)) with Failure e -> Error e

(* Action *)

let first_is_url args =
  if args = [] then false else String.includes ~affix:"://" (List.hd args)

let find_mode env timeout noexec args =
  if first_is_url args
  then server_mode env timeout noexec ~url:(List.hd args) (List.tl args)
  else unit_mode env args

let dyn_units ~args =
  (* XXX This is all very hackish we need a better mecanism.
     Actions should be able to requests units after they parsed their cli. *)
  let args = Cmd.to_list args in
  let is_opt = String.starts_with ~prefix:"-" in
  let args = List.filter (Fun.negate is_opt) args in
  Log.if_error ~use:[] @@
  let unit_mode_units args =
    let units = List.map fst (parse_unit_specs args) in
    B0_unit.get_list_or_hint ~all_if_empty:false units
  in
  let server_mode_unit args =
    let* unit = find_server_mode_unit (List.tl args) in
    match unit with
    | None -> Ok [] | Some (u, _) -> Ok [u]
  in
  if first_is_url args
  then server_mode_unit args
  else unit_mode_units args

let show_url env browser background prefix timeout dry_run no_exec args =
  let secs timeout = Mtime.Span.(timeout * s) in
  Log.if_error ~use:B0_cli.Exit.some_error @@
  let search = B0_env.get_cmd env in
  let* browser = B0_web_browser.find ~search ?cmd:browser () in
  let show url = B0_web_browser.show ~background ~prefix browser url in
  let* mode = find_mode env timeout no_exec args in
  match mode with
  | `Show_url_no_exec (endpoint, timeout, url) ->
      if dry_run
      then (Log.app (fun m -> m "%s" url); Ok B0_cli.Exit.ok) else
      let timeout = secs timeout in
      let* () = Os.Socket.Endpoint.wait_connectable' ~timeout endpoint in
      let* () = show url in
      Ok B0_cli.Exit.ok
  | `Show_url_server (endpoint, timeout, url, cmd, cwd, env) ->
      if dry_run
      then (Log.app (fun m -> m "%a" Cmd.pp cmd); Ok B0_cli.Exit.ok) else
      let* server = Os.Cmd.spawn ~cwd ~env cmd in
      let* () =
        let timeout = secs timeout in
        Os.Socket.Endpoint.wait_connectable' ~timeout endpoint
      in
      let* () = show url in
      let* st = Os.Cmd.spawn_wait_status server in
      let code = match st with `Exited c -> c | `Signaled c -> 128 + c in
      Ok (Os.Exit.code code)
  | `Show_unit_urls urls ->
      let* () =
        if dry_run
        then (Log.app (fun m -> m "@[<v>%a@]" Fmt.(list string) urls); Ok ())
        else List.iter_stop_on_error show urls
      in
      Ok B0_cli.Exit.ok

(* .show-url action command line interface  *)

let show_url_cmd action env =
  let open Cmdliner in
  let doc = "Show URLs of files or servers in browsers" in
  let man =
    [
      `S Manpage.s_synopsis;
      `P "$(iname) [$(i,OPTION)]… $(i,UNIT[:PATH])…";
      `Noblank;
      `P "$(iname) [$(i,OPTION)]… $(i,URL) $(b,--) $(i,ENTITY) [$(i,ARG)]…";
      `S Manpage.s_description;
      `P "$(iname) shows URLs of files in units or requested from \
          built servers.";
      `P "In the first mode of operation for each of the given \
          $(i,UNIT[:PATH]) argument, the $(i,UNIT) is built, $(i,PATH) \
          is made absolute in $(i,UNIT)'s build directory and turned \
          into a $(b,file://) URL. If $(i,PATH) is unspecified the value \
          specified in the unit's $(b,B0_show_url.url) key is used; \
          defaults to the unit's build directory. \
          Consult $(b,b0 get .show-url.url) $(i,UNIT) for \
          the concrete value.";
      `P "In the second mode of operation the given $(i,ENTITY) is built,
          this can be a tool name or an executable UNIT name (TODO eventually
          we should be able to simply have ACTION here, like for build)
          A hostname and port is derived from the authority of $(i,URL) and \
          transformed into command line arguments via the function specified \
          in $(i,ENTITY)'s $(b,B0_show_url.listen_args) key. These argument \
          are added after $(i,ARG)… or before a $(b,--) present in them. \
          The tool is then executed with the arguments, and the environment
          $(b,B0_unit.exec_env) and $(iname) \
          waits for the port to become connectable \
          (this may result in churn in your tool logs) before finally \
          showing the given $(i,URL) in your browser.";
      `Blocks B0_web_browser.man_best_effort_reload;
      `S Manpage.s_see_also;
      `P "Consult $(b,odig doc b0) for the $(b,B0_show_url) \
          module documentation."]
  in
  let args =
    let doc =
      "This is either a list of $(i,UNIT[:PATH]) arguments or an $(i,URL) \
       followed by a tool invocation. The mode of operation is \
       discriminated by looking up for $(b,://) \
       in the first argument to catch an URL."
    in
    Arg.(non_empty & pos_all string [] & info [] ~doc ~docv:"ARGS")
  in
  let timeout =
    let doc = "Maximal number of seconds to wait for the port to \
               become connectable. Default is defined by \
               $(b,b0 get .show-url.timeout) $(i,TOOL)."
    in
    let docv = "SECS" in
    Arg.(value & opt (some int) None & info ["t"; "timeout"] ~doc ~docv)
  in
  let dry_run =
    let doc =
      "Build but do not show the URLs. Print them or print the tool \
       invocation."
    in
    Arg.(value & flag & info ["dry-run"] ~doc)
  in
  let no_exec =
    let doc = "Show URL but do not invoke a tool." in
    Arg.(value & flag & info ["n"; "no-exec"] ~doc)
  in
  Cmd.v (Cmd.info ".show-url" ~doc ~man) @@
  Term.(const show_url $ const env $
        B0_web_browser.browser () $
        B0_web_browser.background () $
        B0_web_browser.prefix ~default:true () $
        timeout $ dry_run $ no_exec $ args)

let action =
  let doc = "Show URLs of files or server runs" in
  B0_action.of_cmdliner_cmd "" ~dyn_units show_url_cmd ~doc

(* Unit exec *)

let unit_exec =
  let exec b0_env ?env ?cwd u ~args =
    let err e = Log.err (fun m -> m "%s" e); Ok B0_cli.Exit.some_error in
    match B0_unit.get_meta (* or default ? *) url u with
    | Error e -> err e
    | Ok url ->
        let* url = get_url b0_env u in
        match B0_env.get_cmd b0_env (Cmd.tool "show-url") with
        | Error e -> err e
        | Ok show_url ->
            let cmd = Cmd.(show_url % url %% args) in
            Ok (Os.Exit.exec ?env ?cwd cmd)
  in
  `Fun ("show-url", exec)


let () = B0_scope.close ()
