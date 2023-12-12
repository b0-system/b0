(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_std.Result.Syntax
open Cmdliner

(* Environment variables *)

module Env = struct
  let pager = "PAGER"
  let less = "LESS"
  let term = "TERM"

  let infos =
    Cmd.Env.info pager
      ~doc:"The pager used to display content. This is a command invocation \
            given to execvp(3)." ::
    Cmd.Env.info term
      ~doc:"See options $(b,--color) and $(b,--no-pager)." :: []
end

(* Paging *)

let find ?win_exe ?search ~don't () =
  if don't then Ok None else
  match Os.Env.find ~empty_is_none:true Env.term with
  | Some "dumb" | None -> Ok None
  | Some _ ->
      let cmds = B0_std.[Cmd.arg "less"; Cmd.arg "more"] in
      let* cmds =
        let empty_is_none = true in
        match Os.Env.find' ~empty_is_none B0_std.Cmd.of_string Env.pager with
        | Error _ as e -> e
        | Ok None -> Ok cmds
        | Ok (Some cmd) -> Ok (cmd :: cmds)
      in
      let rec loop = function
      | [] -> Ok None
      | cmd :: cmds ->
          match Os.Cmd.find ?win_exe ?search cmd with
          | Ok None -> loop cmds
          | v -> v
      in
      loop cmds

let pager_env () = match Os.Env.find ~empty_is_none:false Env.less with
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
            let parent_pid = Unix.getpid () in
            let on_parent_exit () =
              (* We need to be careful here, forked processes will also
                 get execute to this. *)
              if parent_pid = Unix.getpid () then begin
                (* Before closing Unix.stdout it's better to flush
                   formatter and channels. Otherwise it's done later
                   by OCaml's standard shutdown procedure and it
                   raises Sys_error as the fd is no longer valid. *)
                (try Fmt.flush Fmt.stdout () with Sys_error _ -> ());
                (try flush stdout with Sys_error _ -> ());
                (try Unix.close Unix.stdout with Unix.Unix_error _ -> ());
                Log.if_error ~use:() @@ Result.map ignore @@
                Os.Cmd.spawn_wait_status pid
              end
            in
            at_exit on_parent_exit;
            Ok ()

let page_files pager files = match pager with
| Some pager when files = [] -> Ok ()
| Some pager -> Os.Cmd.run B0_std.Cmd.(pager %% paths files)
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

(* Cli interaction *)

let don't ?(docs = Manpage.s_common_options) () =
  let doc =
    "Do not display the output in a pager. This automatically happens \
     if the $(b,TERM) environment variable is $(b,dumb) or undefined."
  in
  Arg.(value & flag & info ["no-pager"] ~docs ~doc)
