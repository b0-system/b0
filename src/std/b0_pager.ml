(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

(* Environment variables *)

module Env = struct
  let pager = "PAGER"
  let less = "LESS"
  let term = "TERM"

  let infos =
    let open Cmdliner in
    Cmd.Env.info pager
      ~doc:"The pager used to display content. This is a command invocation \
            given to execvp(3)." ::
    Cmd.Env.info term
      ~doc:"See option $(b,--no-pager)." :: []
end

(* Paging *)

type t = Cmd.t option
let does_page = Option.is_some

let find ?search ?cmd ~no_pager () =
  if no_pager then Ok None else
  match cmd with
  | Some cmd -> Ok (Os.Cmd.find ?search cmd)
  | None ->
      match Os.Env.var ~empty_is_none:true Env.term with
      | Some "dumb" | None -> Ok None
      | Some _ ->
          let cmds = [Cmd.tool "less"; Cmd.tool "more"] in
          let* cmds =
            let empty_is_none = true in
            match Os.Env.var' ~empty_is_none Cmd.of_string Env.pager with
            | Error _ as e -> e
            | Ok None -> Ok cmds
            | Ok (Some cmd) -> Ok (cmd :: cmds)
          in
          Ok (Os.Cmd.find_first ?search cmds)

let pager_env () = match Os.Env.var ~empty_is_none:false Env.less with
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
            let* pid = Os.Cmd.spawn ?env ~stdin pager in
            let* () = dup2 parent_write Unix.stdout in
            let parent_pid = Unix.getpid () in
            let on_parent_exit () =
              (* We need to be careful here, forked processes will also
                 get to execute this. *)
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
| Some pager -> Os.Cmd.run Cmd.(pager %% paths files)
| None ->
    let rec loop = function
    | [] -> Ok ()
    | f :: fs ->
        match Os.File.read f with
        | Error _ as e -> e
        | Ok contents ->
            Printf.printf "%s" contents;
            if fs <> [] then Printf.printf "\x1C" (* U+001C FS *);
            flush stdout;
            loop fs
    in
    loop files

(* Cli interaction *)

let no_pager ?(docs = Cmdliner.Manpage.s_common_options) () =
  let open Cmdliner in
  let doc =
    "Do not display the output in a pager. This automatically happens \
     if the $(b,TERM) environment variable is $(b,dumb) or undefined."
  in
  Arg.(value & flag & info ["no-pager"] ~docs ~doc_envs:Env.infos ~doc)
