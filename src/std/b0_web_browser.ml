(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

(* macOS JavaScript automation *)

let macos_jxa ?search () = Os.Cmd.find ?search (Cmd.tool "osascript")
let macos_jxa_run jxa script args =
  let cmd = Cmd.(jxa % "-l" % "JavaScript" % "-" %% args) in
  Os.Cmd.run_out ~stdin:(Os.Cmd.in_string script) ~trim:true cmd

let macos_jxa_default_browser_appid jxa =
  Result.map_error (fun e -> Fmt.str "default lookup: %s" e) @@
  macos_jxa_run jxa {|
     ObjC.import('CoreServices');
     var h = $.LSCopyDefaultHandlerForURLScheme($('http'));
     $.CFStringGetCStringPtr (h, 0);
    |} Cmd.empty

(* Finding a browser *)

module Env = struct
  let browser = "BROWSER"
end

type t =
| Cmd of Cmd.t
| Macos_chrome of Cmd.t (* this is osascript *)
| Macos_safari of Cmd.t (* this is osascript *)
| Macos_open of Cmd.t * string option

let browser_env_fallback browser = match browser with
| Some _ as b -> Ok b
| None -> Os.Env.var' ~empty_is_none:true Cmd.of_string Env.browser

let find_browser_cmd ?search cmd = match Os.Cmd.find ?search cmd with
| None -> None | Some c -> Some (Cmd c)

let find_macos_open ?search ~appid () =
  match Os.Cmd.find ?search (Cmd.tool "open") with
  | None -> None
  | Some tool -> Some (Macos_open (tool, appid))

let find_with_macos_jxa ?search ~browser jxa = match browser with
| Some cmd ->
    Result.ok @@
    begin match Cmd.is_singleton cmd with
    | false -> find_browser_cmd ?search cmd
    | true ->
        begin match String.Ascii.lowercase @@ List.hd (Cmd.to_list cmd) with
        | "chrome" -> Some (Macos_chrome jxa)
        | "firefox" ->
            find_macos_open ?search ~appid:(Some "org.mozilla.firefox") ()
        | "open" -> find_macos_open ?search ~appid:None ()
        | "safari" -> Some (Macos_safari jxa)
        | _ -> find_browser_cmd ?search cmd
        end
    end
| None ->
    let* appid = macos_jxa_default_browser_appid jxa in
    Result.ok @@
    match String.Ascii.lowercase appid with
    | "" -> find_macos_open ?search ~appid:None ()
    | "com.apple.safari" -> Some (Macos_safari jxa)
    | "com.google.chrome" -> Some (Macos_chrome jxa)
    | appid -> find_macos_open ?search ~appid:(Some appid) ()

let find ?search ?cmd () =
  Result.map_error (fun e -> Fmt.str "find browser: %s" e) @@
  let* result =
    let* browser = browser_env_fallback cmd in
    match macos_jxa ?search () with
    | Some jxa -> find_with_macos_jxa ?search ~browser jxa
    | None ->
        match browser with
        | Some cmd -> Ok (find_browser_cmd cmd)
        | None ->
            match Os.Cmd.find Cmd.(arg "xdg-open") with
          | None -> Ok None
          | Some xdg -> Ok (Some (Cmd xdg))
  in
  match result with
  | Some b -> Ok b
  | None ->
      Fmt.error "No browser found. Set the %a environment variable."
        Fmt.code Env.browser

(* Show *)

let show_cmd ~background ~prefix cmd url = Os.Cmd.run Cmd.(cmd % url)
let show_macos_open ~background ~prefix:_ open_tool ~appid url =
  let appid = match appid with
  | None -> Cmd.empty
  | Some appid -> Cmd.(arg "-b" % appid)
  in
  let cmd = Cmd.(open_tool %% if' background (arg "-g") %% appid) in
  Os.Cmd.run Cmd.(cmd % url)

let show_macos_jxa name  ~background ~prefix jxa url script =
  let bool = string_of_bool in
  let args = Cmd.(arg (bool background) %% arg (bool prefix) % url) in
  match macos_jxa_run jxa script args with
  | Ok _ -> Ok ()
  | Error e -> Fmt.error "%s jxa: %s" name e

let show_macos_chrome ~background ~prefix jxa url =
  (* It seems we no longer mange to bring the window to front.
     using win.index = 1 doesn't work. Maybe we should only consider
     windows[0]. *)
  show_macos_jxa "chrome" ~background ~prefix jxa url {|
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

let show_macos_safari ~background ~prefix jxa url =
  (* win.index = 1 also (see chrome) doesn't work here and sadly opening
     a directory file URL opens the finder. *)
  show_macos_jxa "safari" ~background ~prefix jxa url {|
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

let show ~background ~prefix browser url =
  Result.map_error (fun e -> Fmt.str "show url %s: %s" url e) @@
  match browser with
  | Cmd cmd -> show_cmd ~background ~prefix cmd url
  | Macos_chrome jxa -> show_macos_chrome ~background ~prefix jxa url
  | Macos_safari jxa -> show_macos_safari ~background ~prefix jxa url
  | Macos_open (open_tool, appid) ->
      show_macos_open ~background ~prefix open_tool ~appid url

(* Cli interaction *)

open Cmdliner

let browser ?docs ?(opts = ["b"; "browser"]) () =
  let env = Cmd.Env.info Env.browser in
  let doc =
    "The WWW browser command $(docv) to use. The value may be interpreted \
     and massaged depending on the OS. On macOS: the names $(b,firefox), \
     $(b,chrome) and $(b,safari) are interpreted specially; use $(b,open) \
     if you would like to use $(b,open\\(2\\)); if absent the application that \
     handles the $(b,http) URL scheme is used. On other platforms if
     $(b,xdg-open\\(1\\)) is found in $(b,PATH) this is the program used by
     default."
  in
  let absent = "OS dependent fallback" in
  let cmd = Arg.conv' ~docv:"CMD" (B0_std.Cmd.of_string, B0_std.Cmd.pp_dump) in
  Arg.(value & opt (Arg.some cmd) None &
       info opts ~absent ~env ~doc ?docs ~docv:"CMD")

let prefix ?docs ~default () =
  let default_str = " (default)" in
  let prefix =
    let opts = ["prefix"] in
    let opts = if not default then "p" :: opts else opts in
    let doc =
      Fmt.str
        "Reload first tab which has the URL as a prefix or create new tab%s. \
         See also $(b,--exact). \
         Platform and browser support for this feature is severly limited."
        (if default then default_str else "")
    in
    true, Arg.info opts ~doc ?docs
  in
  let exact =
    let opts = ["exact"] in
    let doc =
      Fmt.str "Reload first tab which has the exact URL or create new tab%s. \
               See also $(b,--prefix)."
        (if default then "" else default_str)
    in
    false, Arg.info opts ~doc ?docs
  in
  Arg.(value & vflag default [prefix; exact])

let background ?docs ?(opts = ["g"; "background"]) () =
  let doc =
    "Keep launched applications in the background. Platform support for \
     this feature is limited."
  in
  Arg.(value & flag & info opts ~doc ?docs)

let man_best_effort_reload =
  [ `P "Up to severe platform and browser limitation, $(iname) tries to limit \
        the creation of new tabs by reloading existing ones which have the \
        same URL or are prefixed by the URL (see $(b,--prefix))." ]
