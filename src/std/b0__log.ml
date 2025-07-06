(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Reporting levels *)

type level = Quiet | Stdout | Stderr | Error | Warning | Info | Debug
let current_level = Atomic.make Warning
let level () = Atomic.get current_level
let set_level l = Atomic.set current_level l
let level_to_string = function
| Quiet -> "quiet" | Stdout -> "stdout" | Stderr -> "stderr"
| Error -> "error" | Warning -> "warning" | Info -> "info" | Debug -> "debug"

let level_of_string s = match String.trim s with
| "quiet" -> Ok Quiet | "stdout" -> Ok Stdout | "stderr" -> Ok Stderr
| "error" -> Ok Error | "warning" -> Ok Warning | "info" ->  Ok Info
| "debug" ->  Ok Debug
| e ->
    let pp_level = B0__fmt.code in
    let kind = B0__fmt.any "log level" in
    let dom =
      ["quiet"; "stdout"; "stderr"; "error"; "warning"; "info"; "debug"]
    in
    let unknown = B0__fmt.(unknown' ~kind pp_level ~hint:must_be) in
    B0__fmt.error "%a" unknown (e, dom)

(* Default reporter *)

let header_stdout_style = [`Fg `Cyan]
let header_stderr_style = [`Fg `Cyan]
let header_err_style = [`Fg `Red]
let header_warn_style = [`Fg `Yellow]
let header_info_style = [`Fg `Blue]
let header_debug_style = [`Faint; `Fg `Magenta]
let error_style = `Bold :: header_err_style
let warning_style = `Bold :: header_warn_style
let info_style = `Bold :: header_info_style
let debug_style = `Bold :: header_debug_style

let pp_level_header level ppf header = match level with
| Stdout -> B0__fmt.st header_stdout_style ppf header
| Stderr -> B0__fmt.st header_stderr_style ppf header
| Error -> B0__fmt.st header_err_style ppf header
| Warning -> B0__fmt.st header_warn_style ppf header
| Info -> B0__fmt.st header_info_style ppf header
| Debug -> B0__fmt.st header_debug_style ppf header
| Quiet -> ()

let pp_level ppf level = match level with
| Stdout | Stderr -> ()
| Error -> B0__fmt.st error_style ppf "Error"
| Warning -> B0__fmt.st warning_style ppf "Warning"
| Info -> B0__fmt.st info_style ppf "Info"
| Debug -> B0__fmt.st debug_style ppf "Debug"
| Quiet -> ()

let exec =
  (* We use the name as given to execv because sometimes executables
     masquerade as others by execv'ing. We want to use the name of the
     program not of the masquerade (which may be meaningless). *)
  (* assert (Array.length Sys.argv > 0) *)
  Filename.basename Sys.argv.(0)

let default_kmsg k level msgf =
  msgf @@ fun ?header fmt ->
  let ppf = if level = Stdout then B0__fmt.stdout else B0__fmt.stderr in
  let finish ppf =
    Format.pp_close_box ppf (); Format.pp_close_box ppf ();
    B0__fmt.flush_nl ppf ();
    k ()
  in
  Format.pp_open_box ppf 0;
  begin match header with
  | None ->
      begin match level with
      | Stderr | Stdout -> Format.pp_open_box ppf 0
      | level ->
          (* "%s: @[%a: " *)
          B0__fmt.string ppf exec;
          B0__fmt.string ppf ": ";
          Format.pp_open_box ppf 0;
          pp_level ppf level;
          B0__fmt.string ppf ": ";
      end
  | Some header ->
      if header = "" then Format.pp_open_box ppf 0 else begin
        (* "%s: @[[%s] " *)
        B0__fmt.string ppf exec;
        B0__fmt.string ppf ": ";
        Format.pp_open_box ppf 0;
        B0__fmt.char ppf '[';
        pp_level_header level ppf header;
        B0__fmt.string ppf "] ";
      end
  end;
  B0__fmt.kpf finish ppf fmt

(* Log monitoring *)

let err_count' = Atomic.make 0
let err_count () = Atomic.get err_count'
let warn_count' = Atomic.make 0
let warn_count () = Atomic.get warn_count'

(* Log functions *)

type ('a, 'b) msgf =
  (?header:string -> ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

type 'a log = ('a, unit) msgf -> unit
type reporter = { kmsg : 'a 'b. (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b }

let reporter_default = { kmsg = default_kmsg }
let reporter = Atomic.make reporter_default
let kmsg k level msgf = match level with
| Error ->
    Atomic.incr err_count';
    if Atomic.get current_level < Error then k () else
    (Atomic.get reporter).kmsg k level msgf
| Warning ->
    Atomic.incr warn_count';
    if Atomic.get current_level < Warning then k () else
    (Atomic.get reporter).kmsg k level msgf
| _ ->
    let current_level = Atomic.get current_level in
    if current_level = Quiet || current_level < level then k () else
    (Atomic.get reporter).kmsg k level msgf

let kunit _ = ()
let msg level msgf = kmsg kunit level msgf
let quiet msgf = kmsg kunit Quiet msgf
let stdout msgf = kmsg kunit Stdout msgf
let stderr msgf = kmsg kunit Stderr msgf
let err msgf = kmsg kunit Error msgf
let warn msgf = kmsg kunit Warning msgf
let info msgf = kmsg kunit Info msgf
let debug msgf = kmsg kunit Debug msgf

(* Logging result errors *)

let pp_err = B0__fmt.lines

let if_error ?(level = Error) ?header ~use = function
| Ok v -> v
| Error msg -> kmsg (fun _ -> use) level (fun m -> m ?header "%a" pp_err msg)

let if_error' ?(level = Error) ?header ~use = function
| Ok _ as v -> v
| Error msg -> kmsg (fun _ -> Ok use) level (fun m -> m ?header "%a" pp_err msg)

let if_error_pp pp ?(level = Error) ?header ~use = function
| Ok v -> v
| Error e -> kmsg (fun _ -> use) level (fun m -> m ?header "%a" pp e)

let if_error_pp' pp ?(level = Error) ?header ~use = function
| Ok _ as v -> v
| Error e -> kmsg (fun _ -> Ok use) level (fun m -> m ?header "%a" pp e)

(* Logging timings *)

let time ?(level = Info) m f =
  let time = B0__os.Mtime.counter () in
  let v = f () in
  let span = B0__os.Mtime.count time in
  kmsg (fun () -> v) level
    (fun w ->
       let header = Format.asprintf "%a" B0__mtime.Span.pp span in
       m v (w ~header))

(* Spawn logging *)

let spawn_tracer level =
  if level = Quiet then B0__os.Cmd.spawn_tracer_nop else
  let header = function
  | None -> "EXECV"
  | Some pid -> "EXEC:" ^ string_of_int (B0__os.Cmd.pid_to_int pid)
  in
  let pp_env ppf = function
  | None -> () |
    Some env ->
      B0__fmt.pf ppf "%a@," (B0__fmt.list B0__fmt.OCaml.string) env
  in
  fun pid env ~cwd cmd ->
    msg level (fun m ->
        m ~header:(header pid) "@[<v>%a%a@]" pp_env env B0__cmd.pp_dump cmd)

(* Module reporter *)

module Reporter = struct
  type t = reporter =
    { kmsg : 'a 'b. (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b }

  let nop = let kmsg k level msgf = k () in { kmsg }
  let default = reporter_default
  let get () = Atomic.get reporter
  let set r = Atomic.set reporter r
end

(* Exit logging *)

let () =
  B0__os.Exit.log_error :=
    (fun e -> err (fun m -> m "@[%a@]" B0__fmt.lines e))
