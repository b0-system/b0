(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


module Cmd = B0__cmd
module Fmt = B0__fmt
module Fpath = B0__fpath

module Mtime = B0__mtime
module Os = B0__os

module Char = B0__char
module List = B0__list
module Result = B0__result
module String = B0__string
module Type = B0__type

(* Operating system interactions *)

module Log = struct

  (* Reporting levels *)

  type level = Quiet | Stdout | Stderr | Error | Warning | Info | Debug
  let _level = ref Warning
  let level () = !_level
  let set_level l = _level := l
  let level_to_string = function
  | Quiet -> "quiet" | Stdout -> "stdout" | Stderr -> "stderr"
  | Error -> "error" | Warning -> "warning"
  | Info -> "info" | Debug -> "debug"

  let level_of_string s = match String.trim s with
  | "quiet" -> Ok Quiet
  | "stdout" -> Ok Stdout
  | "stderr" -> Ok Stderr
  | "error" -> Ok Error
  | "warning" -> Ok Warning
  | "info" ->  Ok Info
  | "debug" ->  Ok Debug
  | e ->
      let pp_level = Fmt.code in
      let kind = Fmt.any "log level" in
      let dom =
        ["quiet"; "stdout"; "stderr"; "error"; "warning"; "info"; "debug"]
      in
      Fmt.error "%a" Fmt.(unknown' ~kind pp_level ~hint:must_be) (e, dom)

  (* Reporting *)

  let stdout_style = [`Fg `Cyan]
  let stderr_style = [`Fg `Cyan]
  let err_style = [`Fg `Red]
  let warn_style = [`Fg `Yellow]
  let info_style = [`Fg `Blue]
  let debug_style = [`Faint; `Fg `Magenta]

  let pp_level_str level ppf v = match level with
  | Stdout -> Fmt.st stdout_style ppf v
  | Stderr -> Fmt.st stderr_style ppf v
  | Error -> Fmt.st err_style ppf v
  | Warning -> Fmt.st warn_style ppf v
  | Info -> Fmt.st info_style ppf v
  | Debug -> Fmt.st debug_style ppf v
  | Quiet -> assert false

  let pp_level ppf level = match level with
  | Stdout | Stderr -> ()
  | Error -> Fmt.st (`Bold :: err_style) ppf "Error"
  | Warning -> Fmt.st (`Bold :: warn_style) ppf "Warning"
  | Info -> Fmt.st (`Bold :: info_style) ppf "Info"
  | Debug -> Fmt.st (`Bold :: debug_style) ppf "Debug"
  | Quiet -> assert false

  let pp_header =
    let x = match Array.length Sys.argv with
    | 0 -> Filename.basename Sys.executable_name
    | n -> Filename.basename Sys.argv.(0)
    in
    let pp_header ppf (l, h) = match h with
    | None ->
        if l = Stderr || l = Stdout then () else
        Fmt.pf ppf "%s: %a: " x pp_level l
    | Some "" -> ()
    | Some h -> Fmt.pf ppf "%s: [%a] " x (pp_level_str l) h
    in
    pp_header

  (* Log functions *)

  let _err_count = ref 0
  let err_count () = !_err_count

  let _warn_count = ref 0
  let warn_count () = !_warn_count

  type ('a, 'b) msgf =
    (?header:string -> ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

  type 'a log = ('a, unit) msgf -> unit
  type 'a func = { log : 'a. 'a log }

  let log func = func.log

  type kmsg = { kmsg : 'a 'b. (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b }

  let report level k msgf =
    msgf @@ fun ?header fmt ->
    let k _ = k () in
    let ppf =
      if level = Stdout then Format.std_formatter else Format.err_formatter
    in
    Format.kfprintf k ppf ("@[%a" ^^ fmt ^^ "@]@.") pp_header (level, header)

  let kmsg_nop = let kmsg k level msgf = k () in { kmsg }
  let kmsg_default =
    let kmsg k level msgf = match !_level with
    | Quiet -> k ()
    | level' when level > level' || level = Quiet ->
        (if level = Error then incr _err_count else
         if level = Warning then incr _warn_count else ());
        (k ())
    | _ ->
        (if level = Error then incr _err_count else
         if level = Warning then incr _warn_count else ());
        report level k msgf
    in
    { kmsg }

  let _kmsg = ref kmsg_default
  let set_kmsg kmsg = _kmsg := kmsg

  let kunit _ = ()
  let msg level msgf = !_kmsg.kmsg kunit level msgf
  let quiet msgf = !_kmsg.kmsg kunit Quiet msgf
  let stdout msgf = !_kmsg.kmsg kunit Stdout msgf
  let stderr msgf = !_kmsg.kmsg kunit Stderr msgf
  let err msgf = !_kmsg.kmsg kunit Error msgf
  let warn msgf = !_kmsg.kmsg kunit Warning msgf
  let info msgf = !_kmsg.kmsg kunit Info msgf
  let debug msgf = !_kmsg.kmsg kunit Debug msgf
  let kmsg k level msgf = !_kmsg.kmsg k level msgf

  (* Logging result errors *)

  let if_error ?(level = Error) ?header ~use = function
  | Ok v -> v
  | Error msg ->
      !_kmsg.kmsg (fun _ -> use) level @@ fun m ->
      m ?header "@[%a@]" Fmt.lines msg

  let if_error' ?(level = Error) ?header ~use = function
  | Ok _ as v -> v
  | Error msg ->
      !_kmsg.kmsg (fun _ -> Ok use) level @@ fun m ->
      m ?header "@[%a@]" Fmt.lines msg

  let if_error_pp ?(level = Error) ?header pp ~use = function
  | Ok v -> v
  | Error e ->
      !_kmsg.kmsg (fun _ -> use) level @@ fun m -> m ?header "@[%a@]" pp e

  let if_error_pp' ?(level = Error) ?header pp ~use = function
  | Ok _ as v -> v
  | Error e ->
      !_kmsg.kmsg (fun _ -> Ok use) level @@ fun m -> m ?header "@[%a@]" pp e

  (* Timing logging *)

  let time ?(level = Info) m f =
    let time = Os.Mtime.counter () in
    let r = f () in
    let span = Os.Mtime.count time in
    !_kmsg.kmsg (fun () -> r) level
      (fun w ->
         let header = Format.asprintf "%a" Mtime.Span.pp span in
         m r (w ~header))

  (* Spawn logging *)

  let spawn_tracer level =
    if level = Quiet then Os.Cmd.spawn_tracer_nop else
    let header = function
    | None -> "EXECV"
    | Some pid -> "EXEC:" ^ string_of_int (Os.Cmd.pid_to_int pid)
    in
    let pp_env ppf = function
    | None -> () | Some env -> Fmt.pf ppf "%a@," (Fmt.list Fmt.OCaml.string) env
    in
    fun pid env ~cwd cmd ->
      msg level (fun m ->
          m ~header:(header pid) "@[<v>%a%a@]" pp_env env Cmd.pp_dump cmd)

  (* Exit logging *)

  let () =
    Os.Exit.log_error := (fun e -> err (fun m -> m "@[%a@]" Fmt.lines e))
end

module Fut = struct
  type 'a state = Det of 'a | Undet of { mutable awaits : ('a -> unit) list }
  type 'a t = 'a state ref

  let rec kontinue ks v =
    let todo = ref ks in
    while match !todo with [] -> false | _ -> true do
      match !todo with k :: ks -> todo := ks; k v | [] -> ()
    done

  let set f v = match !f with
  | Det _ -> invalid_arg "The future is already set"
  | Undet u -> f := Det v; kontinue u.awaits v

  let _make () = ref (Undet { awaits = [] })
  let make () = let f = _make () in f, set f
  let value f = match !f with Det v -> Some v | _ -> None
  let await f k = match !f with
  | Det v -> k v | Undet u -> u.awaits <- k :: u.awaits

  let rec sync f = match !f with
  | Det v -> v
  | Undet _ -> Os.relax (); sync f

  let return v = ref (Det v)

  let map fn f =
    let r = _make () in
    await f (fun v -> set r (fn v)); r

  let bind f fn =
    let r = _make () in
    await f (fun v -> await (fn v) (set r)); r

  let pair f0 f1 =
    let r = _make () in
    await f0 (fun v0 -> await f1 (fun v1 -> set r (v0, v1))); r

  let of_list fs = match fs with
  | [] -> return []
  | fs ->
      let r = _make () in
      let rec loop acc = function
      | [] -> set r (List.rev acc)
      | f :: fs -> await f (fun v -> loop (v :: acc) fs)
      in
      loop [] fs; r

  module Syntax = struct
    let ( let* ) = bind
    let ( and* ) = pair
  end
end

module Bval = struct
  let already_set () = invalid_arg "already set"

  type 'a t =
  | V of 'a
  | Lazy of 'a Fut.t * (unit -> unit)
  | Fut of ('a Fut.t * ('a -> unit))

  type 'a setter = 'a t
  let make () = let bv = Fut (Fut.make ()) in bv, bv
  let of_val v = V v
  let of_lazy_fun f =
    (* XXX stir should spawn a fiber. *)
    let value, set = Fut.make () in
    let run = ref true in
    let stir () = if !run then (run := true; set (f ())) else () in
    Lazy (value, stir)

  let of_setter = Fun.id
  let is_lazy = function Lazy _ -> true | _ -> false

  (* Setting *)

  let set s v = match s with
  | Fut (fut, set) -> set v
  | _ -> assert false

  let try_set s v = match s with
  | Fut (fut, set) ->
      (match Fut.value fut with None -> set v; true | Some _ -> false)
  | _ -> assert false

  (* Getting *)

  let get = function
  | V v -> Fut.return v
  | Lazy (fut, stir) -> stir (); fut
  | Fut (fut, _) -> fut

  let poll = function
  | V v -> Some v
  | Lazy (fut, stir) -> stir (); Fut.value fut
  | Fut (fut, _) -> Fut.value fut

  let stir = function Lazy (_, stir) -> stir () | _ -> ()

  (* Formatting *)

  let pp pp_v ppf = function
  | V v -> pp_v ppf v
  | Lazy (fut, _) | Fut (fut, _) ->
      match Fut.value fut with
      | None -> Fmt.string ppf "<pending>" | Some v -> pp_v ppf v
end
