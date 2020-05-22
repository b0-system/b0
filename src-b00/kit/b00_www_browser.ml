(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open Cmdliner

(* macOS JavaScript automation *)

let macos_jxa ?search () = Os.Cmd.find_tool ?search (Fpath.v "osascript")

let macos_jxa_run jxa script cli_args =
  let stdin = Os.Cmd.in_string script in
  let cmd = Cmd.(path jxa % "-l" % "JavaScript" % "-" %% cli_args) in
  Os.Cmd.(run_out ~stdin ~trim:true cmd)

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
| Macos_chrome of Cmd.tool (* this is osascript *)
| Macos_safari of Cmd.tool (* this is osascript *)
| Macos_open of Cmd.tool * string option

let browser_env_fallback browser = match browser with
| Some _ as b -> Ok b
| None -> Os.Env.find' ~empty_is_none:true Cmd.of_string Env.browser

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
    Result.bind (macos_jxa_default_browser_appid jxa) @@ fun appid ->
    match String.Ascii.lowercase appid with
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

(* Cli interaction *)

let browser ?docs ?(opts = ["browser"]) () =
  let env = Arg.env_var Env.browser in
  let doc =
    "The WWW browser command $(docv) to use. The value may be interpreted \
     and massaged depending on the OS. On macOS: the names $(b,firefox), \
     $(b,chrome) and $(b,safari) are interpreted specially; use $(b,open) \
     if you would like to use $(b,open\\(2\\)); if absent the application that \
     handles the $(b,http) URI scheme is used. On other platforms if
     $(b,xdg-open\\(1\\)) is found in $(b,PATH) this is the program used by
     default."
  in
  let cmd = Arg.some ~none:"OS dependent fallback" B00_std_ui.cmd in
  Arg.(value & opt cmd None & info opts ~env ~doc ?docs ~docv:"CMD")

let prefix ?docs ?(opts = ["prefix"]) () =
  let doc =
    "Rather than the exact URI, reload if possible, the first browser tab \
     which has the URI as a prefix. Platform and browser support for this \
     feature is severly limited."
  in
  Arg.(value & flag & info opts ~doc ?docs)

let background ?docs ?(opts = ["background"]) () =
  let doc =
    "Keep launched applications in the background. Platform support for \
     this feature is limited."
  in
  Arg.(value & flag & info opts ~doc ?docs)

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
