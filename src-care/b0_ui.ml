(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std

module Cli = struct
  open Cmdliner

  module Arg = struct
    let err_msg of_string s = Result.map_error (fun e -> `Msg e) (of_string s)
    let fpath = Arg.conv ~docv:"PATH" (err_msg Fpath.of_string, Fpath.pp_quoted)
    let cmd = Arg.conv ~docv:"CMD" (err_msg Cmd.of_string, Cmd.dump)
  end

  (* Specifying output formats *)

  type out_fmt = [ `Normal | `Short | `Long ]
  let out_fmt
      ?docs ?(short_opts = ["s"; "short"]) ?(long_opts = ["l"; "long"]) ()
    =
    let short =
      let doc = "Short output. Line based output with only relevant data." in
      Cmdliner.Arg.info short_opts ~doc ?docs
    in
    let long =
      let doc = "Long output. Outputs as much information as possible." in
      Cmdliner.Arg.info long_opts ~doc ?docs
    in
    Cmdliner.Arg.(value & vflag `Normal [`Short, short; `Long, long])
end

module B0_std = struct
  open Cmdliner

  let color ?(docs = Manpage.s_common_options) ?env () =
    let enum = ["auto", None; "always", Some `Ansi; "never", Some `None] in
    let color = Arg.enum enum in
    let enum_alts = Arg.doc_alts_enum enum in
    let doc = Fmt.str "Colorize the output. $(docv) must be %s." enum_alts in
    let docv = "WHEN" in
    Arg.(value & opt color None & info ["color"] ?env ~doc ~docv ~docs)

  let verbosity ?(docs = Manpage.s_common_options) ?env () =
    let vopts =
      let doc =
        "Increase verbosity. Repeatable, but more than twice does \
         not bring more. Takes over $(b,--verbosity)."
       (* The reason for taking over verbosity is due to cmdliner
          limitation: we cannot distinguish in choose below if it was
          set via an env var. And cli args should always take over env
          var. So verbosity set through the env var would take over -v
          otherwise. *)
      in
      Arg.(value & flag_all & info ["v"; "verbose"] ~doc ~docs)
    in
    let verbosity =
      let enum =
        [ "warning", None; (* Hack for the option's absent rendering *)
          "quiet", Some Log.Quiet;
          "error", Some Log.Error;
          "warning", Some Log.Warning;
          "info", Some Log.Info;
          "debug", Some Log.Debug; ]
      in
      let log_level = Arg.enum enum in
      let enum_alts = Arg.doc_alts_enum List.(tl enum) in
      let doc =
        Fmt.str "Be more or less verbose. $(docv) must be %s." enum_alts
      in
      Arg.(value & opt log_level None &
           info ["verbosity"] ?env ~docv:"LEVEL" ~doc ~docs)
      in
      let quiet =
        let doc = "Be quiet. Takes over $(b,-v) and $(b,--verbosity)." in
        Arg.(value & flag & info ["q"; "quiet"] ~doc ~docs)
      in
      let choose quiet verbosity vopts =
        if quiet then Log.Quiet else match vopts with
        | (_ :: []) -> Log.Info
        | ( _:: _ :: _) -> Log.Debug
        | [] ->
            match verbosity with
            | Some verbosity -> verbosity
            | None -> Log.Warning
      in
      Term.(const choose $ quiet $ verbosity $ vopts)

  let log_spawn level =
    let header = function
    | None -> "EXECV"
    | Some pid -> "EXEC:" ^ string_of_int (Os.Cmd.pid_to_int pid)
    in
    let pp_env ppf = function
    | None -> ()
    | Some env -> Fmt.pf ppf "%a@," (Fmt.list String.dump) env
    in
    fun pid env ~cwd cmd ->
      Log.msg level
        (fun m -> m ~header:(header pid) "@[<v>%a%a@]" pp_env env Cmd.dump cmd)

  let setup_log_spawns = function
  | Log.Quiet -> ()
  | level -> Os.Cmd.set_spawn_tracer (log_spawn level)

  let cli_setup ?docs ?(log_spawns = Log.Debug) ?color_env ?verbosity_env () =
    let color = color ?docs ?env:color_env () in
    let verbosity = verbosity ?docs ?env:verbosity_env () in
    let setup color verbosity =
      let cap = match color with
      | None -> Tty.cap (Tty.of_fd Unix.stdout)
      | Some cap -> cap
      in
      Fmt.set_tty_styling_cap cap;
      B0_std.Log.set_level verbosity;
      setup_log_spawns log_spawns;
    in
    Term.(const setup $ color $ verbosity)
end

module File_cache = struct

  (* High-level commands *)

  let delete ~dir keys = Result.bind (Os.Dir.exists dir) @@ function
  | false -> Ok ()
  | true ->
      match keys with
      | `All ->
          Result.bind (Os.Path.delete ~recurse:true dir) @@ fun _ ->
          Result.bind (Os.Dir.create ~make_path:true dir) @@ (* recreate dir *)
          fun _ -> Ok ()
      | `Keys keys ->
          Result.bind (B00.File_cache.create dir) @@ fun c ->
          let rec loop = function
          | [] -> Ok ()
          | k :: ks ->
              match B00.File_cache.rem c k with
              | Error _ as e -> e
              | Ok true -> loop ks
              | Ok false ->
                  Log.warn (fun m -> m "%s: no such key in cache, ignored" k);
                  loop ks
          in
          loop keys

  let gc ~dir = Result.bind (Os.Dir.exists dir) @@ function
  | false -> Ok ()
  | true ->
      Result.bind (B00.File_cache.create dir) @@ fun c ->
      Result.bind (B00.File_cache.delete_unused c) @@ fun () -> Ok ()

  let size ~dir =
    let stats = Result.bind (Os.Dir.exists dir) @@ function
    | false -> Ok B00.File_cache.Stats.zero
    | true ->
        Result.bind (B00.File_cache.create dir) @@ fun c ->
        B00.File_cache.Stats.of_cache c
    in
    Result.bind stats @@ fun stats ->
    Log.app (fun m -> m "@[<v>%a@]" B00.File_cache.Stats.pp stats); Ok ()

  let trim ~dir ~max_byte_size ~pct =
    Result.bind (Os.Dir.exists dir) @@ function
    | false -> Ok ()
    | true ->
        Result.bind (B00.File_cache.create dir) @@ fun c ->
        B00.File_cache.trim_size c ~max_byte_size ~pct

  (* Cli fragments *)

  open Cmdliner

  let key_arg =
    let of_string s = match Fpath.is_seg s with
    | true -> Ok s
    | false -> Error (`Msg "Not a valid key (not a path segment)")
    in
    Arg.conv (of_string, String.pp) ~docv:"KEY"

  let keys_none_is_all ?(pos_right = -1) () =
    let doc =
      "Select key $(docv) (repeatable). If unspecified selects all keys."
    in
    let keys = Arg.(value & pos_right 0 key_arg [] & info [] ~doc ~docv:"KEY")in
    Term.(const (function [] -> `All | ks -> `Keys ks) $ keys)
end

module Memo = struct
  open Cmdliner

  let b0_dir_name = "_b0"
  let cache_dir_name = ".cache"
  let trash_dir_name = ".trash"
  let b0_dir_env = "B0_DIR"
  let cache_dir_env = "B0_CACHE_DIR"
  let b0_dir
      ?(docs = Manpage.s_common_options)
      ?(doc = "Use $(docv) for the b0 directory.")
      ?(doc_none = "$(b,_b0) in root directory")
      ?(env = Cmdliner.Arg.env_var b0_dir_env) ()
    =
    Arg.(value & opt (some ~none:doc_none Cli.Arg.fpath) None &
         info ["b0-dir"] ~env ~doc ~docs ~docv:"DIR")

  let cache_dir
      ?(docs = Manpage.s_common_options)
      ?(doc = "Use $(docv) for the build cache directory.")
      ?(doc_none = "$(b,.cache) in b0 directory")
      ?(env = Cmdliner.Arg.env_var cache_dir_env) ()
    =
    Arg.(value & opt (some ~none:doc_none Cli.Arg.fpath) None &
         info ["cache-dir"] ~env ~doc ~docs ~docv:"DIR")

  let get_b0_dir ~cwd ~root ~b0_dir = match b0_dir with
  | None -> Fpath.(root / b0_dir_name)
  | Some d -> Fpath.(cwd // d)

  let get_cache_dir ~cwd ~b0_dir ~cache_dir = match cache_dir with
  | None -> Fpath.(b0_dir / cache_dir_name)
  | Some d -> Fpath.(cwd // d)

  let jobs ?docs ?env () =
    let doc = "Maximal number of commands to spawn concurrently." in
    let docv = "COUNT" in
    Arg.(value & opt (some int) None & info ["j"; "jobs"] ?env ~doc ?docs ~docv)

  let max_spawn ~jobs () = match jobs with
  | Some max -> max
  | None ->
      let cpu_count = B0_machine.logical_cpu_count () in
      let cpu_count = Log.if_error ~level:Log.Warning ~use:None cpu_count in
      Option.value ~default:1 cpu_count
end

module Pager = struct
  open Cmdliner

  let envs =
    Term.env_info "PAGER"
      ~doc:"The pager used to display content. This is a command \
            invocation given to execvp(3)." ::
    Term.env_info "TERM"
      ~doc:"See options $(b,--color) and $(b,--no-pager)." :: []

  let don't ?docs () =
    let doc =
      "Do not display the output in a pager. This automatically happens \
       if the $(b,TERM) environment variable is $(b,dumb) or unset."
    in
    Arg.(value & flag & info ["no-pager"] ?docs ~doc)

  let find ?search ~don't () = match don't with
  | true -> Ok None
  | false ->
      match Os.Env.find ~empty_to_none:true "TERM" with
      | Some "dumb" | None -> Ok None
      | Some _ ->
          let cmds = [Cmd.arg "less"; Cmd.arg "more"] in
          let cmds =
            match Os.Env.find_value Cmd.of_string ~empty_to_none:true "PAGER"
            with
            | None -> Ok cmds
            | Some (Ok cmd) -> Ok (cmd :: cmds)
            | Some (Error _ as e) -> e
          in
          Result.bind cmds (Os.Cmd.find_first ?search)

  let pager_env () = match Os.Env.find ~empty_to_none:false "LESS" with
  | Some _ -> Ok None
  | None ->
      Result.bind (Os.Env.current_assignments ()) @@ fun env ->
      Ok (Some ("LESS=FRX" :: env))

  let page_stdout = function
  | None -> Ok ()
  | Some pager ->
      let uerr = Unix.error_message in
      let err fmt = Fmt.error ("page stdout: " ^^ fmt) in
      let rec dup2 fd0 fd1 = match Unix.dup2 fd0 fd1 with
      | () -> Ok ()
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> dup2 fd0 fd1
      | exception Unix.Unix_error (e, _, _) -> err "dup2: %s" (uerr e)
      in
      match pager_env () with
      | Error e -> err "%s" e
      | Ok env ->
          match Unix.pipe () with
          | exception Unix.Unix_error (e, _, _)  -> err "pipe: %s" (uerr e)
          | (pager_read, parent_write) ->
              let stdin = Os.Cmd.in_fd ~close:true pager_read in
              Unix.set_close_on_exec parent_write;
              Os.Fd.apply ~close:Unix.close parent_write @@ fun parent_write ->
              Result.bind (Os.Cmd.spawn ?env ~stdin pager) @@ fun pid ->
              Result.bind (dup2 parent_write Unix.stdout) @@ fun () ->
              let on_exit () =
                (* Before closing Unix.stdout it's better to flush
                   formatter and channels. Otherwise it's done later
                   by OCaml's standard shutdown procedure and it
                   raises Sys_error as the fd is no longer valid. *)
                (try Fmt.flush Fmt.stdout () with Sys_error _ -> ());
                (try flush stdout with Sys_error _ -> ());
                (try Unix.close Unix.stdout with Unix.Unix_error _ -> ());
                (Result.map (fun st -> ()) (Os.Cmd.spawn_wait_status pid)
                 |> Log.if_error ~use:())
              in
              Pervasives.at_exit on_exit;
              Ok ()

  let page_files pager files = match pager with
  | Some pager when files = [] -> Ok ()
  | Some pager -> Os.Cmd.run Cmd.(pager %% paths files)
  | None ->
      let rec loop = function
      | [] -> Ok ()
      | f :: fs ->
          match Os.File.read f with
          | Error _ as e -> e
          | Ok d ->
              Printf.printf "%s" d;
              if fs <> [] then Printf.printf "\x1C" (* U+001C FS *);
              flush stdout;
              loop fs
      in
      loop files
end

module Editor = struct
  open Cmdliner

  let envs =
    Term.env_info "VISUAL"
      ~doc:"The editor used to edit files. This is a command \
            invocation given to execvp(3) and is used before EDITOR." ::
    Term.env_info "EDITOR"
      ~doc:"The editor used to edit files. This is a command \
            invocation given to execvp(3) and is used after VISUAL." ::
    []

  let find ?search () =
    let parse_env cmds env = match cmds with
    | Error _ as e -> e
    | Ok cmds as r ->
        match Os.Env.find_value Cmd.of_string ~empty_to_none:true env with
        | None -> r
        | Some (Ok cmd) -> Ok (cmd :: cmds)
        | Some (Error _ as e) -> e
    in
    let cmds = Ok [Cmd.arg "nano"] in
    let cmds = parse_env cmds "EDITOR" in
    let cmds = parse_env cmds "VISUAL" in
    Result.bind cmds (Os.Cmd.find_first ?search)

  let edit_files editor fs = match editor with
  | None -> Error "No runnable editor found in VISUAL or EDITOR"
  | Some editor -> Os.Cmd.run_status Cmd.(editor %% paths fs)
end

module Pdf_viewer = struct
  open Cmdliner

  (* XXX support background *)

  (* Cli *)

  let pdf_viewer_var = "PDFVIEWER"
  let pdf_viewer ?docs ?(opts = ["pdf-viewer"]) () =
    let env = Arg.env_var pdf_viewer_var in
    let doc =
      "The PDF viewer command $(docv) to use. If absent either one \
       of $(b,xdg-open(1)) or $(b,open(1)) is used. If not found and \
       on Windows $(b,start) is used."
    in
    let cmd = Arg.some ~none:"OS dependent fallback" Cli.Arg.cmd in
    Arg.(value & opt cmd None & info opts ~env ~doc ?docs ~docv:"CMD")

  (* Viewer *)

  type t = Cmd.t

  let find ?search ~pdf_viewer () =
    Result.map_error (fun e -> Fmt.str "find PDF viewer: %s" e) @@
    match pdf_viewer with
    | Some cmd -> Os.Cmd.find ?search cmd
    | None ->
        Result.bind (Os.Cmd.find ?search Cmd.(arg "xdg-open")) @@ function
        | Some xdg -> Ok (Some xdg)
        | None ->
            Result.bind (Os.Cmd.find ?search Cmd.(arg "open")) @@ function
            | Some oopen -> Ok (Some oopen)
            | None ->
                if Sys.win32
                then Ok (Some Cmd.(arg "start" % "")) (* really ? *)
                else Ok None

  let show pdf_viewer file =
    Result.map_error
      (fun e -> Fmt.str "show PDF %a: %s" Fpath.pp_quoted file e) @@
    match pdf_viewer with
    | None -> Error "No PDF viewer found, use the PDFVIEWER env var to set one."
    | Some cmd -> Os.Cmd.run Cmd.(cmd %% path file)
end

module Browser = struct
  open Cmdliner

  (* Cli *)

  let browser_var = "BROWSER"
  let browser ?docs ?(opts = ["browser"]) () =
    let env = Arg.env_var browser_var in
    let doc =
      "The WWW browser command $(docv) to use. The value may be interpreted \
       and massaged depending on the OS. On macOS: the names $(b,firefox), \
       $(b,chrome) and $(b,safari) are interpreted specially; use $(b,open) \
       if you would like to use open(2); if absent the application that \
       handles the $(b,http) URI scheme is used. On other platforms if
       xdg-open(1) is found in $(b,PATH) this the program used by default."
    in
    let cmd = Arg.some ~none:"OS dependent fallback" Cli.Arg.cmd in
    Arg.(value & opt cmd None & info opts ~env ~doc ?docs ~docv:"CMD")

  let prefix ?docs ?(opts = ["p"; "prefix"]) () =
    let doc =
      "Rather than the exact URI, reload if possible, the first browser tab \
       which has the URI as a prefix. Platform and browser support for this \
       feature is severly limited."
    in
    Arg.(value & flag & info opts ~doc ?docs)

  let background ?docs ?(opts = ["g"; "background"]) () =
    let doc =
      "Keep launched applications in the background. Platform support for \
       this feature is limited."
    in
    Arg.(value & flag & info opts ~doc ?docs)

  (* macOS JavaScript automation *)

  let macos_jxa ?search () = Os.Cmd.find_tool ?search (Fpath.v "osascript")

  let macos_jxa_run jxa script cli_args =
    let stdin = Os.Cmd.in_string script in
    let cmd = Cmd.(path jxa % "-l" % "JavaScript" % "-" %% cli_args) in
    Os.Cmd.(run_out ~stdin cmd)

  let macos_jxa_default_browser_appid jxa =
    Result.map_error (fun e -> Fmt.str "default lookup: %s" e) @@
    macos_jxa_run jxa {|
     ObjC.import('CoreServices');
     var h = $.LSCopyDefaultHandlerForURLScheme($('http'));
     $.CFStringGetCStringPtr (h, 0);
    |} Cmd.empty

  (* Finding a browser *)

  type t =
  | Cmd of Cmd.t
  | Macos_chrome of Cmd.tool (* this is osascript *)
  | Macos_safari of Cmd.tool (* this is osascript *)
  | Macos_open of Cmd.tool * string option

  let browser_env_fallback browser = match browser with
  | Some _ as b -> Ok b
  | None ->
      match Os.Env.find_value Cmd.of_string ~empty_to_none:true browser_var with
      | None -> Ok None
      | Some (Ok b) -> Ok (Some b)
      | Some (Error _ as e) -> e

  let find_browser_cmd ?search cmd =
    Result.bind (Os.Cmd.find ?search cmd) @@ function
    | None -> Ok None
    | Some c -> Ok (Some (Cmd c))

  let find_macos_open ?search ~appid =
    Result.bind (Os.Cmd.find_tool ?search (Fpath.v "open")) @@ function
    | None -> Ok None
    | Some tool -> Ok (Some (Macos_open (tool, appid)))

  let find_with_macos_jxa ?search ~browser jxa = match browser with
  | Some cmd when not (Cmd.is_singleton cmd) -> find_browser_cmd ?search cmd
  | Some cmd ->
      begin match String.Ascii.lowercase @@ List.hd (Cmd.to_list cmd) with
      | "chrome" -> Ok (Some (Macos_chrome jxa))
      | "firefox" -> find_macos_open ?search ~appid:(Some "org.mozilla.firefox")
      | "open" -> find_macos_open ?search ~appid:None
      | "safari" -> Ok (Some (Macos_safari jxa))
      | _ -> find_browser_cmd ?search cmd
      end
  | None ->
      Result.bind (macos_jxa_default_browser_appid jxa) @@ function
      | "" -> find_macos_open ?search ~appid:None
      | "com.apple.safari" -> Ok (Some (Macos_safari jxa))
      | "com.google.chrome" -> Ok (Some (Macos_chrome jxa))
      | appid -> find_macos_open ?search ~appid:(Some appid)

  let find ?search ~browser () =
    Result.map_error (fun e -> Fmt.str "find browser: %s" e) @@
    Result.bind (browser_env_fallback browser) @@ fun browser ->
    Result.bind (macos_jxa ?search ()) @@ function
    | Some jxa -> find_with_macos_jxa ?search ~browser jxa
    | None ->
        match browser with
        | Some cmd -> find_browser_cmd cmd
        | None ->
            Result.bind (Os.Cmd.find Cmd.(arg "xdg-open")) @@ function
            | None -> Ok None
            | Some xdg -> Ok (Some (Cmd xdg))

  (* Show *)

  let show_cmd ~background ~prefix cmd uri = Os.Cmd.run Cmd.(cmd % uri)
  let show_macos_open ~background ~prefix:_ open_tool ~appid uri =
    let appid = match appid with
    | None -> Cmd.empty
    | Some appid -> Cmd.(arg "-b" % appid)
    in
    let cmd = Cmd.(path open_tool %% if' background (arg "-g") %% appid) in
    Os.Cmd.run Cmd.(cmd % uri)

  let show_macos_jxa name  ~background ~prefix jxa uri script =
    let bool = string_of_bool in
    let args = Cmd.(arg (bool background) %% arg (bool prefix) % uri) in
    match macos_jxa_run jxa script args with
    | Ok _ -> Ok ()
    | Error e -> Fmt.error "%s jxa: %s" name e

  let show_macos_chrome ~background ~prefix jxa uri =
    (* It seems we no longer mange to bring the window to front.
       using win.index = 1 doesn't work. Maybe we should only consider
       windows[0]. *)
    show_macos_jxa "chrome" ~background ~prefix jxa uri {|
function is_equal (s0, s1) { return s0 === s1; }
function is_prefix (p, s) { return s && s.lastIndexOf (p, 0) === 0; }
function run(argv) {
  var background = (argv[0] == 'true');
  var pred = (argv[1] == 'true') ? is_prefix : is_equal;
  var uri = argv[2];
  var app = Application ('com.google.chrome');
  if (!background) app.activate ();
  for (var w = 0; w < app.windows.length; w++) {
    var win = app.windows[w];
    var tab = win.activeTab;
	  if (pred (uri, tab.url ())) { app.reload (tab); return; }
	  for (var t = 0; t < win.tabs.length; t++) {
		  tab = win.tabs[t];
		  if (pred (uri, tab.url ())) {
		   app.reload (tab); win.activeTabIndex = t + 1; return;
		  }
	  }
  }
  if (app.windows.length == 0) { app.Window().make(); }
  if (app.windows[0].activeTab.url () === 'chrome://newtab/')
    { app.windows[0].activeTab.url = uri; }
  else { app.windows[0].tabs.push(app.Tab ({ url : uri })); }
}|}

  let show_macos_safari ~background ~prefix jxa uri =
    (* win.index = 1 also (see chrome) doesn't work here and sadly opening
       a directory file URI opens the finder. *)
    show_macos_jxa "safari" ~background ~prefix jxa uri {|
function is_equal (s0, s1) { return s0 === s1; }
function is_prefix (p, s) { return s && s.lastIndexOf (p, 0) === 0; }
function run(argv) {
  var background = (argv[0] == 'true');
  var pred = (argv[1] == 'true') ? is_prefix : is_equal;
  var uri = argv[2];
  var app = Application ('com.apple.safari');
  if (!background) app.activate ();
  for (var w = 0; w < app.windows.length; w++) {
    var win = app.windows[w];
    var tab = win.currentTab;
	  if (pred (uri, tab.url ())) { tab.url = tab.url(); return; }
	  for (var t = 0; t < win.tabs.length; t++) {
      tab = win.tabs[t];
	    if (pred (uri, tab.url ()))
      { tab.url = tab.url(); win.currentTab = tab; return; }
	  }
  }
  if (app.windows.length == 0) { app.Document().make(); }
  if (app.windows[0].currentTab.url () === null)
    { app.windows[0].currentTab.url = uri; }
  else { app.windows[0].tabs.push(app.Tab ({ url : uri }));
         app.windows[0].currentTab =
         app.windows[0].tabs[app.windows[0].tabs.length-1]; }
}|}

  let show ~background ~prefix browser uri =
    Result.map_error (fun e -> Fmt.str "show uri %s: %s" uri e) @@
    match browser with
    | None -> Error "No browser found. Use the BROWSER env var to set one."
    | Some b ->
        match b with
        | Cmd cmd -> show_cmd ~background ~prefix cmd uri
        | Macos_chrome jxa -> show_macos_chrome ~background ~prefix jxa uri
        | Macos_safari jxa -> show_macos_safari ~background ~prefix jxa uri
        | Macos_open (open_tool, appid) ->
            show_macos_open ~background ~prefix open_tool ~appid uri
end


(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
