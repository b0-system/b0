(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () = B0_scope.open_lib ~module':__MODULE__ "show-url"

open B0_std
open Result.Syntax

(* Keys *)

let path =
  let doc = "The default path to show, relative to the unit's directory." in
  B0_meta.Key.make "path" ~doc ~pp_value:Fpath.pp

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

(* Action *)

let get_tool_or_hint tool tool_map =
  (* XXX should we have that in String.Map perhaps ? *)
  match String.Map.find_opt tool tool_map with
  | Some v -> Ok v
  | None ->
      let suggs =
        let add_sugg k v acc =
          if String.edit_distance k tool <= 2 then k :: acc else acc
        in
        List.rev (String.Map.fold add_sugg tool_map [])
      in
      let kind ppf () = Fmt.pf ppf "%s" "tool" in
      let hint = Fmt.did_you_mean in
      let pp = Fmt.unknown' ~kind Fmt.code' ~hint in
      Fmt.error "@[%a@]" pp (tool, suggs)

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
  match B0_http.Url.scheme url with
  | None -> Fmt.error "URL %s: no scheme found" url
  | Some "http" -> Os.Socket.Endpoint.of_string ~default_port:80 authority
  | Some "https" -> Os.Socket.Endpoint.of_string ~default_port:443 authority
  | Some scheme ->
      (Log.warn @@ fun m ->
       m "Unknown scheme %a using 80 as the default port" Fmt.code' scheme);
      Os.Socket.Endpoint.of_string ~default_port:80 authority

let server_mode env timeout_cli no_exec ~url args =
  match B0_http.Url.authority url with
  | None -> Fmt.error "Could not extract authority from %s" url
  | Some authority ->
      let* endpoint = endpoint_of_url_and_authority url authority in
      match args with
      | [] ->
          if not no_exec then
            (Log.warn @@ fun m ->
             m "@[<v>No tool specified but trying to connect to %s@,\
                Use option %a to suppress this warning.@]"
               authority Fmt.code' "--no-exec");
          let timeout = match timeout_cli with
          | Some timeout -> timeout
          | None -> B0_meta.Key.get_default timeout_s
          in
          Ok (`Show_url_no_exec (endpoint, timeout, url))
      | entity :: args ->
          (* XXX at somepoint we should be able to invoke the same
             logic as [B0_cmd_build.find_store_and_execution]. *)
          (* XXX We might actually want to look into actions aswell.
             See the todo.mld for more details.
             FIXME This is quite messy we need to clarify executable
             units ideally we should be using B0_unit.find_exec *)
          let* unit =
            let keep = B0_unit.tool_is_user_accessible in
            match B0_unit.get_or_suggest_tool ~keep entity with
            | Ok us ->
                (* FIXME let u = check_tool_ambiguities name us in *)
                Ok (List.hd us)
            | Error tool_suggs ->
                let u = B0_unit.get_or_suggest entity in
                match u with
                | Ok u -> Ok u
                | Error us ->
                    let tname u =
                      Option.get (B0_unit.find_meta B0_unit.tool_name u)
                    in
                    let ts = List.rev_map tname tool_suggs in
                    let us = List.rev_map B0_unit.name us in
                    let set = String.Set.of_list (List.concat [ts; us]) in
                    let suggs = String.Set.elements set in
                    let hint = Fmt.did_you_mean in
                    let nothing_to ppf v =
                      Fmt.pf ppf "Nothing to execute for %a." Fmt.code' v
                    in
                    let pp ppf (v, hints) = match hints with
                    | [] -> nothing_to ppf v
                    | hints ->
                        Fmt.pf ppf "%a@ %a" nothing_to v (hint Fmt.code') hints
                    in
                    Fmt.error "@[%a@]" pp (entity, suggs)
          in
          (* FIXME this should execute according to the build unit execution
             protocol. *)
          let* cmd = B0_env.unit_cmd env unit in
          let timeout = match timeout_cli with
          | Some timeout -> timeout
          | None -> B0_unit.find_or_default_meta timeout_s unit
          in
          let listen_args =
            (B0_unit.find_or_default_meta listen_args unit) ~authority
          in
          let cmd = make_server_cmd cmd args ~listen_args in
          let cwd = Fut.sync (B0_unit.get_exec_cwd env unit) in
          let env' = Fut.sync (B0_unit.get_exec_env env unit) in
          let env' = Option.map Os.Env.to_assignments env' in
          Ok (`Show_url_server (endpoint, timeout, url, cmd, cwd, env'))

let unit_file_mode env args =
  let parse_arg arg = match String.cut_left ~sep:":" arg with
  | None -> arg, (arg, None)
  | Some (uname, path) -> uname, (arg, Some path)
  in
  let specs = List.map parse_arg args in
  let unit_names, paths = List.split specs in
  let* units = B0_unit.get_list_or_hint ~all_if_empty:false unit_names in
  let specs = List.combine units paths in
  let make_url (unit, (arg, p)) =
    Result.error_to_failure @@
    Result.map_error (fun e -> Fmt.str "%a: %s" Fmt.code' arg e) @@
    let build_dir = B0_build.build_dir (B0_env.build env) unit in
    let* path = match p with
    | None -> Ok (B0_unit.find_meta path unit)
    | Some p -> Result.map Option.some (Fpath.of_string p)
    in
    let path = match path with
    | None -> build_dir | Some p -> Fpath.(build_dir // p)
    in
    (* FIXME remote build the build env *)
    let* exists = Os.Path.exists path in
    if not exists
    then Fmt.error "%a: Not such file" Fpath.pp path
    else Ok (Fmt.str "file://%s" (Fpath.to_string path))
  in
  try Ok (`Show_file_urls (List.map make_url specs)) with Failure e -> Error e

let find_mode env timeout noexec args =
  let is_url = String.includes ~affix:"://" (List.hd args) in
  if is_url
  then server_mode env timeout noexec ~url:(List.hd args) (List.tl args)
  else unit_file_mode env args

let show_url env browser prefix background timeout dry_run no_exec args =
  (* XXX need to fix search argument of B0_web_browser and lookup in build *)
  let secs timeout = Mtime.Span.(timeout * s) in
  Log.if_error ~use:B0_cli.Exit.some_error @@
  let* browser = B0_web_browser.find ~browser () in
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
      let* server = Os.Cmd.spawn ?cwd ?env cmd in
      let* () =
        let timeout = secs timeout in
        Os.Socket.Endpoint.wait_connectable' ~timeout endpoint
      in
      let* () = show url in
      let* st = Os.Cmd.spawn_wait_status server in
      let code = match st with `Exited c -> c | `Signaled c -> 128 + c in
      Ok (Os.Exit.code code)
  | `Show_file_urls urls ->
      let* () =
        if dry_run
        then (Log.app (fun m -> m "@[<v>%a@]" Fmt.(list string) urls); Ok ())
        else List.iter_stop_on_error show urls
      in
      Ok B0_cli.Exit.ok

(* .show-url action command line interface  *)

open Cmdliner

let doc = "Show URLs of files or servers in browsers"

let show_url_cmd action env =
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
          specified in the unit's $(b,B0_show_url.path) key is used; \
          defaults to the unit's build directory. \
          Consult $(b,b0 get .show-url.path) $(i,UNIT) for \
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
  B0_action.of_cmdliner_cmd "" show_url_cmd ~doc

let () = B0_scope.close ()
