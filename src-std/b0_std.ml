(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* ANSI terminal interaction *)

module Tty = struct

  (* Terminals *)

  type t = [ `Dumb | `Term of string] option
  let of_fd fd =
    let rec isatty fd = try Unix.isatty fd with
    | Unix.Unix_error (Unix.EINTR, _, _) -> isatty fd
    | Unix.Unix_error (e, _, _) -> false
    in
    if not (isatty fd) then None else
    match Unix.getenv "TERM" with
    | "" -> None
    | "dumb" -> (Some `Dumb)
    | v -> Some (`Term v)
    | exception Not_found -> None

  (* Capabilities *)

  type cap = [ `None | `Ansi ]
  let cap tty = match tty with None | (Some `Dumb) -> `None | _ -> `Ansi

  (* ANSI escapes and styling *)

  type color =
  [ `Default | `Black | `Red | `Green | `Yellow | `Blue | `Magenta | `Cyan
  | `White ]

  let rec sgr_base_int_of_color = function
  | `Black -> 0 | `Red -> 1 | `Green -> 2 | `Yellow -> 3  | `Blue -> 4
  | `Magenta -> 5 | `Cyan -> 6 | `White -> 7 | `Default -> 9
  | `Hi (#color as c) -> 60 + sgr_base_int_of_color c

  let sgr_of_fg_color c = Printf.sprintf "%d" (30 + sgr_base_int_of_color c)
  let sgr_of_bg_color c = Printf.sprintf "%d" (40 + sgr_base_int_of_color c)

  type style =
  [ `Bold | `Faint | `Italic | `Underline | `Blink of [ `Rapid | `Slow ]
  | `Reverse | `Fg of [ color | `Hi of color ]
  | `Bg of [ color | `Hi of color ] ]

  let sgr_of_style = function
  | `Bold -> "01"
  | `Faint -> "02"
  | `Italic -> "03"
  | `Underline -> "04"
  | `Blink `Slow -> "05"
  | `Blink `Rapid -> "06"
  | `Reverse -> "07"
  | `Fg c -> sgr_of_fg_color c
  | `Bg c -> sgr_of_bg_color c

  let sgrs_of_styles styles = String.concat ";" (List.map sgr_of_style styles)

  let styled_str cap styles s = match cap with
  | `None -> s
  | `Ansi -> Printf.sprintf "\027[%sm%s\027[m" (sgrs_of_styles styles) s

  let strip_escapes s =
    let len = String.length s in
    let b = Buffer.create len in
    let max = len - 1 in
    let flush start stop = match start < 0 || start > max with
    | true -> ()
    | false -> Buffer.add_substring b s start (stop - start + 1)
    in
    let rec skip_esc i = match i > max with
    | true -> loop i i
    | false -> let k = i + 1 in if s.[i] = 'm' then loop k k else skip_esc k
    and loop start i = match i > max with
    | true ->
        if Buffer.length b = len then s else
        (flush start max; Buffer.contents b)
    | false ->
        match s.[i] with
        | '\027' -> flush start (i - 1); skip_esc (i + 1)
        | _ -> loop start (i + 1)
    in
    loop 0 0
end

(* Formatters *)

module Fmt = struct

  (* Standard outputs and formatters *)

  let stdout = Format.std_formatter
  let stderr = Format.err_formatter

  (* Formatting *)

  let pf = Format.fprintf
  let pr = Format.printf
  let epr = Format.eprintf
  let str = Format.asprintf
  let kpf = Format.kfprintf
  let kstr = Format.kasprintf
  let failwith fmt = kstr failwith fmt
  let failwith_notrace fmt = kstr (fun s -> raise_notrace (Failure s)) fmt
  let invalid_arg fmt = kstr invalid_arg fmt
  let error fmt = kstr (fun s -> Error s) fmt

  (* Formatters *)

  type 'a t = Format.formatter -> 'a -> unit

  let flush ppf _ = Format.pp_print_flush ppf ()
  let flush_nl ppf _ = Format.pp_print_newline ppf ()
  let nop ppf _ = ()
  let any fmt ppf _ = pf ppf fmt
  let using f pp_v ppf v = pp_v ppf (f v)

  (* Separators *)

  let cut ppf _ = Format.pp_print_cut ppf ()
  let sp ppf _ = Format.pp_print_space ppf ()
  let sps n ppf _ = Format.pp_print_break ppf n 0
  let comma ppf _ = Format.pp_print_string ppf ","; sp ppf ()
  let semi ppf _ = Format.pp_print_string ppf ";"; sp ppf ()

  (* Sequencing *)

  let iter ?sep:(pp_sep = cut) iter pp_elt ppf v =
    let is_first = ref true in
    let pp_elt v =
      if !is_first then (is_first := false) else pp_sep ppf ();
      pp_elt ppf v
    in
    iter pp_elt v

  let iter_bindings ?sep:(pp_sep = cut) iter pp_binding ppf v =
    let is_first = ref true in
    let pp_binding k v =
      if !is_first then (is_first := false) else pp_sep ppf ();
      pp_binding ppf (k, v)
    in
    iter pp_binding v

  let append pp_v0 pp_v1 ppf v = pp_v0 ppf v; pp_v1 ppf v
  let ( ++ ) = append
  let concat ?sep pps ppf v =
    iter ?sep List.iter (fun ppf pp -> pp ppf v) ppf pps

  (* Boxes *)

  let box ?(indent = 0) pp_v ppf v =
    Format.(pp_open_box ppf indent; pp_v ppf v; pp_close_box ppf ())

  let hbox pp_v ppf v =
    Format.(pp_open_hbox ppf (); pp_v ppf v; pp_close_box ppf ())

  let vbox ?(indent = 0) pp_v ppf v =
    Format.(pp_open_vbox ppf indent; pp_v ppf v; pp_close_box ppf ())

  let hvbox ?(indent = 0) pp_v ppf v =
    Format.(pp_open_hvbox ppf indent; pp_v ppf v; pp_close_box ppf ())

  let hovbox ?(indent = 0) pp_v ppf v =
    Format.(pp_open_hovbox ppf indent; pp_v ppf v; pp_close_box ppf ())

  (* Brackets *)

  let surround s1 s2 pp_v ppf v =
    Format.(pp_print_string ppf s1; pp_v ppf v; pp_print_string ppf s2)

  let parens pp_v = box ~indent:1 (surround "(" ")" pp_v)
  let brackets pp_v = box ~indent:1 (surround "[" "]" pp_v)
  let oxford_brackets pp_v = box ~indent:2 (surround "[|" "|]" pp_v)
  let braces pp_v = box ~indent:1 (surround "{" "}" pp_v)
  let quote ?(mark = "\"") pp_v =
    let pp_mark ppf _ = Format.pp_print_as ppf 1 mark in
    box ~indent:1 (pp_mark ++ pp_v ++ pp_mark)

  (* Stdlib formatters *)

  let bool = Format.pp_print_bool
  let int = Format.pp_print_int
  let int32 ppf i = pf ppf "%ld" i
  let int64 ppf i = pf ppf "%Ld" i
  let float ppf f = pf ppf "%g" f
  let char = Format.pp_print_char
  let string = Format.pp_print_string
  let sys_signal ppf snum =
    let sigs = [
      Sys.sigabrt, "SIGABRT"; Sys.sigalrm, "SIGALRM"; Sys.sigfpe, "SIGFPE";
      Sys.sighup, "SIGHUP"; Sys.sigill, "SIGILL"; Sys.sigint, "SIGINT";
      Sys.sigkill, "SIGKILL"; Sys.sigpipe, "SIGPIPE"; Sys.sigquit, "SIGQUIT";
      Sys.sigsegv, "SIGSEGV"; Sys.sigterm, "SIGTERM"; Sys.sigusr1, "SIGUSR1";
      Sys.sigusr2, "SIGUSR2"; Sys.sigchld, "SIGCHLD"; Sys.sigcont, "SIGCONT";
      Sys.sigstop, "SIGSTOP"; Sys.sigtstp, "SIGTSTP"; Sys.sigttin, "SIGTTIN";
      Sys.sigttou, "SIGTTOU"; Sys.sigvtalrm, "SIGVTALRM";
      Sys.sigprof, "SIGPROF"; Sys.sigbus, "SIGBUS"; Sys.sigpoll, "SIGPOLL";
      Sys.sigsys, "SIGSYS"; Sys.sigtrap, "SIGTRAP"; Sys.sigurg, "SIGURG";
      Sys.sigxcpu, "SIGXCPU"; Sys.sigxfsz, "SIGXFSZ"; ]
    in
    try string ppf (List.assoc snum sigs) with
    | Not_found -> pf ppf "SIG(%d)" snum

  let exn ppf e = string ppf (Printexc.to_string e)
  let exn_backtrace ppf (e, bt) =
    let pp_backtrace_str ppf s =
      let stop = String.length s - 1 (* there's a newline at the end *) in
      let rec loop left right =
        if right = stop then string ppf (String.sub s left (right - left)) else
        if s.[right] <> '\n' then loop left (right + 1) else
        begin
          string ppf (String.sub s left (right - left));
          cut ppf ();
          loop (right + 1) (right + 1)
        end
      in
      if s = "" then (string ppf "No backtrace available.") else
      loop 0 0
    in
    pf ppf "@[<v>Exception: %a@,%a@]"
      exn e pp_backtrace_str (Printexc.raw_backtrace_to_string bt)

  let pair ?sep:(pp_sep = cut) pp_fst pp_snd ppf (fst, snd) =
    pp_fst ppf fst; pp_sep ppf (); pp_snd ppf snd

  let option ?none:(pp_none = nop) pp_v ppf = function
  | None -> pp_none ppf ()
  | Some v -> pp_v ppf v

  let none ppf () = string ppf "<none>"

  let list ?(empty = nop) ?sep:pp_sep pp_elt ppf = function
  | [] -> empty ppf ()
  | l -> Format.pp_print_list ?pp_sep pp_elt ppf l

  let array ?(empty = nop) ?sep pp_elt ppf a = match Array.length a with
  | 0 -> empty ppf ()
  | n -> iter ?sep Array.iter pp_elt ppf a

  (* Magnitudes *)

  let ilog10 x =
    let rec loop p x = if x = 0 then p else loop (p + 1) (x / 10) in
    loop (-1) x

  let ipow10 n =
    let rec loop acc n = if n = 0 then acc else loop (acc * 10) (n - 1) in
    loop 1 n

  let si_symb_max = 16
  let si_symb =
    [| "y"; "z"; "a"; "f"; "p"; "n"; "u"; "m"; ""; "k"; "M"; "G"; "T"; "P";
       "E"; "Z"; "Y"|]

  let rec pp_at_factor ~scale u symb factor ppf s =
    let m = s / factor in
    let n = s mod factor in
    match m with
    | m when m >= 100 -> (* No fractional digit *)
        let m_up = if n > 0 then m + 1 else m in
        if m_up >= 1000 then si_size ~scale u ppf (m_up * factor) else
        pf ppf "%d%s%s" m_up symb u
    | m when m >= 10 -> (* One fractional digit w.o. trailing 0 *)
        let f_factor = factor / 10 in
        let f_m = n / f_factor in
        let f_n = n mod f_factor in
        let f_m_up = if f_n > 0 then f_m + 1 else f_m in
        begin match f_m_up with
        | 0 -> pf ppf "%d%s%s" m symb u
        | f when f >= 10 -> si_size ~scale u ppf (m * factor + f * f_factor)
        | f -> pf ppf "%d.%d%s%s" m f symb u
        end
    | m -> (* Two or zero fractional digits w.o. trailing 0 *)
        let f_factor = factor / 100 in
        let f_m = n / f_factor in
        let f_n = n mod f_factor in
        let f_m_up = if f_n > 0 then f_m + 1 else f_m in
        match f_m_up with
        | 0 -> pf ppf "%d%s%s" m symb u
        | f when f >= 100 -> si_size ~scale u ppf (m * factor + f * f_factor)
        | f when f mod 10 = 0 -> pf ppf "%d.%d%s%s" m (f / 10) symb u
        | f -> pf ppf "%d.%02d%s%s" m f symb u

  and si_size ~scale u ppf s = match scale < -8 || scale > 8 with
  | true -> invalid_arg "~scale is %d, must be in [-8;8]" scale
  | false ->
      let pow_div_3 = if s = 0 then 0 else (ilog10 s / 3) in
      let symb = (scale + 8) + pow_div_3 in
      let symb, factor = match symb > si_symb_max with
      | true -> si_symb_max, ipow10 ((8 - scale) * 3)
      | false -> symb, ipow10 (pow_div_3 * 3)
      in
      if factor = 1
      then pf ppf "%d%s%s" s si_symb.(symb) u
      else pp_at_factor ~scale u si_symb.(symb) factor ppf s

  let byte_size ppf s = si_size ~scale:0 "B" ppf s

  (* XXX From 4.08 on use Int64.unsigned_*

     See Hacker's Delight for the implementation of these unsigned_* funs *)

  let unsigned_compare x0 x1 = Int64.(compare (sub x0 min_int) (sub x1 min_int))
  let unsigned_div n d = match d < Int64.zero with
  | true -> if unsigned_compare n d < 0 then Int64.zero else Int64.one
  | false ->
      let q = Int64.(shift_left (div (shift_right_logical n 1) d) 1) in
      let r = Int64.(sub n (mul q d)) in
      if unsigned_compare r d >= 0 then Int64.succ q else q

  let unsigned_rem n d = Int64.(sub n (mul (unsigned_div n d) d))

  let us_span   =                  1_000L
  let ms_span   =              1_000_000L
  let sec_span  =          1_000_000_000L
  let min_span  =         60_000_000_000L
  let hour_span =       3600_000_000_000L
  let day_span  =     86_400_000_000_000L
  let year_span = 31_557_600_000_000_000L

  let rec pp_si_span unit_str si_unit si_higher_unit ppf span =
    let geq x y = unsigned_compare x y >= 0 in
    let m = unsigned_div span si_unit in
    let n = unsigned_rem span si_unit in
    match m with
    | m when geq m 100L -> (* No fractional digit *)
        let m_up = if Int64.equal n 0L then m else Int64.succ m in
        let span' = Int64.mul m_up si_unit in
        if geq span' si_higher_unit then uint64_ns_span ppf span' else
        pf ppf "%Ld%s" m_up unit_str
    | m when geq m 10L -> (* One fractional digit w.o. trailing zero *)
        let f_factor = unsigned_div si_unit 10L in
        let f_m = unsigned_div n f_factor in
        let f_n = unsigned_rem n f_factor in
        let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
        begin match f_m_up with
        | 0L -> pf ppf "%Ld%s" m unit_str
        | f when geq f 10L ->
            uint64_ns_span ppf Int64.(add (mul m si_unit) (mul f f_factor))
        | f -> pf ppf "%Ld.%Ld%s" m f unit_str
        end
    | m -> (* Two or zero fractional digits w.o. trailing zero *)
        let f_factor = unsigned_div si_unit 100L in
        let f_m = unsigned_div n f_factor in
        let f_n = unsigned_rem n f_factor in
        let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
        match f_m_up with
        | 0L -> pf ppf "%Ld%s" m unit_str
        | f when geq f 100L ->
            uint64_ns_span ppf Int64.(add (mul m si_unit) (mul f f_factor))
        | f when Int64.equal (Int64.rem f 10L) 0L ->
            pf ppf "%Ld.%Ld%s" m (Int64.div f 10L) unit_str
        | f ->
            pf ppf "%Ld.%02Ld%s" m f unit_str

  and pp_non_si unit_str unit unit_lo_str unit_lo unit_lo_size ppf span =
    let geq x y = unsigned_compare x y >= 0 in
    let m = unsigned_div span unit in
    let n = unsigned_rem span unit in
    if Int64.equal n 0L then pf ppf "%Ld%s" m unit_str else
    let f_m = unsigned_div n unit_lo in
    let f_n = unsigned_rem n unit_lo in
    let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
    match f_m_up with
    | f when geq f unit_lo_size ->
        uint64_ns_span ppf Int64.(add (mul m unit) (mul f unit_lo))
    | f ->
        pf ppf "%Ld%s%Ld%s" m unit_str f unit_lo_str

  and uint64_ns_span ppf span =
    let geq x y = unsigned_compare x y >= 0 in
    let lt x y = unsigned_compare x y = -1 in
    match span with
    | s when lt s us_span -> pf ppf "%Ldns" s
    | s when lt s ms_span -> pp_si_span "us" us_span ms_span ppf s
    | s when lt s sec_span -> pp_si_span "ms" ms_span sec_span ppf s
    | s when lt s min_span -> pp_si_span "s" sec_span min_span ppf s
    | s when lt s hour_span -> pp_non_si "min" min_span "s" sec_span 60L ppf s
    | s when lt s day_span -> pp_non_si "h" hour_span "min" min_span 60L ppf s
    | s when lt s year_span -> pp_non_si "d" day_span "h" hour_span 24L ppf s
    | s ->
        let m = unsigned_div s year_span in
        let n = unsigned_rem s year_span in
        if Int64.equal n 0L then pf ppf "%Lda" m else
        let f_m = unsigned_div n day_span in
        let f_n = unsigned_rem n day_span in
        let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
        match f_m_up with
        | f when geq f 366L -> pf ppf "%Lda" (Int64.succ m)
        | f -> pf ppf "%Lda%Ldd" m f

  (* Text *)

  let text = Format.pp_print_text
  let lines ppf s =
    let rec stop_at sat ~start ~max s =
      if start > max then start else
      if sat s.[start] then start else
      stop_at sat ~start:(start + 1) ~max s
    in
    let sub s start stop ~max =
      if start = stop then "" else
      if start = 0 && stop > max then s else
      String.sub s start (stop - start)
    in
    let is_nl c = c = '\n' in
    let max = String.length s - 1 in
    let rec loop start s = match stop_at is_nl ~start ~max s with
    | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
    | stop ->
        Format.pp_print_string ppf (sub s start stop ~max);
        Format.pp_force_newline ppf ();
        loop (stop + 1) s
    in
    loop 0 s

  let truncated ~max ppf s = match String.length s <= max with
  | true -> Format.pp_print_string ppf s
  | false ->
      for i = 0 to max - 4 do Format.pp_print_char ppf s.[i] done;
      Format.pp_print_string ppf "..."

  (* HCI fragments *)

  let one_of ?(empty = nop) pp_v ppf = function
  | [] -> empty ppf ()
  | [v] -> pp_v ppf v
  | [v0; v1] -> pf ppf "@[either %a or@ %a@]" pp_v v0 pp_v v1
  | _ :: _ as vs ->
      let rec loop ppf = function
      | [v] -> pf ppf "or@ %a" pp_v v
      | v :: vs -> pf ppf "%a,@ " pp_v v; loop ppf vs
      | [] -> assert false
      in
      pf ppf "@[one@ of@ %a@]" loop vs

  let did_you_mean
      ?(pre = any "Unknown") ?(post = nop) ~kind pp_v ppf (v, hints)
    =
    match hints with
    | [] -> pf ppf "@[%a %s %a%a.@]" pre () kind pp_v v post ()
    | hints ->
        pf ppf "@[%a %s %a%a.@ Did you mean %a ?@]"
          pre () kind pp_v v post () (one_of pp_v) hints

  (* ANSI TTY styling

     XXX What we are doing here is less subtle than what we did in Fmt
     where capability was associated to formatters and hence could
     distinguish between stdout/stderr, maybe we should do that again. *)

  let _tty_styling_cap = ref `None
  let set_tty_styling_cap cap = _tty_styling_cap := cap
  let tty_styling_cap () = !_tty_styling_cap

  let tty_string styles ppf s = match !_tty_styling_cap with
  | `None -> Format.pp_print_string ppf s
  | `Ansi ->
      Format.fprintf ppf "@<0>%s%s@<0>%s"
        (Printf.sprintf "\027[%sm" @@ Tty.sgrs_of_styles styles) s "\027[m"

  let tty styles pp_v ppf v = match !_tty_styling_cap with
  | `None -> pp_v ppf v
  | `Ansi ->
      (* XXX This doesn't compose well, we should get the current state
         and restore it afterwards rather than resetting. *)
      let reset ppf = Format.fprintf ppf "@<0>%s" "\027[m" in
      Format.kfprintf reset ppf "@<0>%s%a"
        (Printf.sprintf "\027[%sm" @@ Tty.sgrs_of_styles styles) pp_v v

  (* Records *)

  external id : 'a -> 'a = "%identity"
  let field
      ?(label = tty_string [`Fg `Yellow]) ?(sep = any ":@ ") l prj pp_v ppf v
    =
    pf ppf "@[<1>%a%a%a@]" label l sep () pp_v (prj v)

  let record ?(sep = cut) pps = vbox (concat ~sep pps)
end

(* Option values *)

module Option = struct

  (* Options *)

  type 'a t = 'a option = None | Some of 'a
  let none = None
  let some v = Some v
  let value o ~default = match o with Some v -> v | None -> default
  let get = function Some v -> v | None -> invalid_arg "option is None"
  let bind o f = match o with None -> None | Some v -> f v
  let join = function Some (Some _ as o) -> o | _ -> None
  let map f o = match o with None -> None | Some v -> Some (f v)
  let fold ~none ~some = function Some v -> some v | None -> none
  let iter f = function Some v -> f v | None -> ()

  (* Predicates and comparisons *)

  let is_none = function None -> true | Some _ -> false
  let is_some = function None -> false | Some _ -> true
  let equal eq o0 o1 = match o0, o1 with
  | Some v0, Some v1 -> eq v0 v1
  | None, None -> true
  | _ -> false

  let compare cmp o0 o1 = match o0, o1 with
  | Some v0, Some v1 -> cmp v0 v1
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1

  (* Converting *)

  let to_result ~none = function None -> Error none | Some v -> Ok v
  let to_list = function None -> [] | Some v -> [v]
end

(* Result values *)

module Result = struct

  (* Results *)

  type ('a, 'e) t = ('a, 'e) result = Ok of 'a | Error of 'e
  let ok v = Ok v
  let error e = Error e
  let value r ~default = match r with Ok v -> v | Error _ -> default
  let get_ok = function Ok v -> v | Error _ -> invalid_arg "result is Error _"
  let get_error = function Error e -> e | Ok _ -> invalid_arg "result is Ok _"
  let bind r f = match r with Ok v -> f v | Error _ as e -> e
  let join = function Ok r -> r | Error _ as e -> e
  let map f = function Ok v -> Ok (f v) | Error _ as e -> e
  let map_error f = function Error e -> Error (f e) | Ok _ as v -> v
  let fold ~ok ~error = function Ok v -> ok v | Error e -> error e
  let iter f = function Ok v -> f v | Error _ -> ()
  let iter_error f = function Error e -> f e | Ok _ -> ()

  (* Predicates and comparisons *)

  let is_ok = function Ok _ -> true | Error _ -> false
  let is_error = function Error _ -> true | Ok _ -> false
  let equal ~ok ~error r0 r1 = match r0, r1 with
  | Ok v0, Ok v1 -> ok v0 v1
  | Error e0, Error e1 -> error e0 e1
  | _, _ -> false

  let compare ~ok ~error r0 r1 = match r0, r1 with
  | Ok v0, Ok v1 -> ok v0 v1
  | Error e0, Error e1 -> error e0 e1
  | Ok _, Error _ -> -1
  | Error _, Ok _ -> 1

  (* Interacting with Stdlib exceptions *)

  let to_failure = function Ok v -> v | Error e -> failwith e
  let catch_failure f = try Ok (f ()) with Failure e -> Error e
  let catch_sys_error f = try Ok (f ()) with Sys_error e -> Error e

  (* Converting *)

  let to_option = function Ok v -> Some v | Error _ -> None
  let to_list = function Ok v -> [v] | Error _ -> []
end

(* Characters *)

module Char = struct
  include Char
  module Ascii = struct
    let max = '\x7F'

    (* Decimal and hexadecimal digits *)

    let is_digit = function '0' .. '9' -> true | _ -> false
    let is_hex_digit = function
    | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
    | _ -> false

    let hex_digit_value = function
    | '0' .. '9' as c -> Char.code c - 0x30
    | 'A' .. 'F' as c -> 10 + (Char.code c - 0x41)
    | 'a' .. 'f' as c -> 10 + (Char.code c - 0x61)
    | c -> Fmt.invalid_arg "%C: not a hex digit" c

    let lower_hex_digit n =
      let n = n land 0xF in
      Char.unsafe_chr (if n < 10 then 0x30 + n else 0x57 + n)

    let upper_hex_digit n =
      let n = n land 0xF in
      Char.unsafe_chr (if n < 10 then 0x30 + n else 0x37 + n)

    (* Predicates *)

    let is_valid : t -> bool = fun c -> c <= max
    let is_upper = function 'A' .. 'Z' -> true | _ -> false
    let is_lower = function 'a' .. 'z' -> true | _ -> false
    let is_letter = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false
    let is_alphanum = function
    | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false

    let is_white = function ' ' | '\t' .. '\r'  -> true | _ -> false
    let is_blank = function ' ' | '\t' -> true | _ -> false
    let is_graphic = function '!' .. '~' -> true | _ -> false
    let is_print = function ' ' .. '~' -> true | _ -> false
    let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false

    (* Casing transforms *)

    let uppercase = function 'a' .. 'z' as c -> chr (code c - 0x20) | c -> c
    let lowercase = function 'A' .. 'Z' as c -> chr (code c + 0x20) | c -> c
  end
end

(* Strings *)

module String = struct
  include String

  let empty = ""
  let head s = if s = "" then None else Some s.[0]
  let of_char c = String.make 1 c

  (* Predicates *)

  let is_empty s = equal empty s
  let is_prefix ~affix s =
    let len_a = String.length affix in
    let len_s = String.length s in
    if len_a > len_s then false else
    let max_idx_a = len_a - 1 in
    let rec loop i =
      if i > max_idx_a then true else
      if unsafe_get affix i <> unsafe_get s i then false else loop (i + 1)
    in
    loop 0

  let is_infix ~affix s =
    let len_a = String.length affix in
    let len_s = String.length s in
    if len_a > len_s then false else
    let max_idx_a = len_a - 1 in
    let max_idx_s = len_s - len_a in
    let rec loop i k =
      if i > max_idx_s then false else
      if k > max_idx_a then true else
      if k > 0 then
        if unsafe_get affix k = unsafe_get s (i + k)
        then loop i (k + 1) else loop (i + 1) 0
      else
        if unsafe_get affix 0 = unsafe_get s i
        then loop i 1 else loop (i + 1) 0
    in
    loop 0 0

  let is_suffix ~affix s =
    let max_idx_a = String.length affix - 1 in
    let max_idx_s = String.length s - 1 in
    if max_idx_a > max_idx_s then false else
    let rec loop i =
      if i > max_idx_a then true else
      if unsafe_get affix (max_idx_a - i) <> unsafe_get s (max_idx_s - i)
      then false
      else loop (i + 1)
    in
    loop 0

  let for_all sat s =
    let max_idx = String.length s - 1 in
    let rec loop i =
      if i > max_idx then true else
      if sat (unsafe_get s i) then loop (i + 1) else false
    in
    loop 0

  let exists sat s =
    let max_idx = String.length s - 1 in
    let rec loop i =
      if i > max_idx then false else
      if sat (unsafe_get s i) then true else loop (i + 1)
    in
    loop 0

  (* Extracting substrings *)

  let with_index_range ?(first = 0) ?last s =
    let max = String.length s - 1 in
    let last = match last with
    | None -> max
    | Some l when l > max -> max
    | Some l -> l
    in
    let first = if first < 0 then 0 else first in
    if first > last then "" else
    String.sub s first (last - first + 1)

  (* Breaking with magnitudes *)

  let take_left n s = with_index_range ~last:(n - 1) s
  let drop_left n s = with_index_range ~first:n s
  let break_left n s = (take_left n s, drop_left n s)
  let take_right n s = with_index_range ~first:(String.length s - n) s
  let drop_right n s = with_index_range ~last:(String.length s - n - 1) s
  let break_right n s = (drop_right n s, take_right n s)

  (* Breaking with predicates *)

  let keep_left sat s =
    let max = String.length s - 1 in
    let rec loop max s i = match i > max with
    | true -> s
    | false when sat s.[i] -> loop max s (i + 1)
    | false -> with_index_range ~last:(i - 1) s
    in
    loop max s 0

  let lose_left sat s =
    let max = String.length s - 1 in
    let rec loop max s i = match i > max with
    | true -> ""
    | false when sat s.[i] -> loop max s (i + 1)
    | false -> with_index_range ~first:i s
    in
    loop max s 0

  let span_left sat s =
    let max = String.length s - 1 in
    let rec loop max s i = match i > max with
    | true -> s, ""
    | false when sat s.[i] -> loop max s (i + 1)
    | false -> with_index_range ~last:(i - 1) s, with_index_range ~first:i s
    in
    loop max s 0

  let keep_right sat s =
    let max = String.length s - 1 in
    let rec loop s i = match i < 0 with
    | true -> s
    | false when sat s.[i] -> loop s (i - 1)
    | false -> with_index_range ~first:(i + 1) s
    in
    loop s max

  let lose_right sat s =
    let max = String.length s - 1 in
    let rec loop s i = match i < 0 with
    | true -> ""
    | false when sat s.[i] -> loop s (i - 1)
    | false -> with_index_range ~last:i s
    in
    loop s max

  let span_right sat s =
    let max = String.length s - 1 in
    let rec loop s i = match i < 0 with
    | true -> "", s
    | false when sat s.[i] -> loop s (i - 1)
    | false -> with_index_range ~last:i s, with_index_range ~first:(i + 1) s
    in
    loop s max

  (* Breaking with separators *)

  let err_empty_sep = "~sep is an empty string"

  let cut_left ~sep s =
    let sep_len = length sep in
    if sep_len = 0 then invalid_arg err_empty_sep else
    let s_len = length s in
    let max_sep_idx = sep_len - 1 in
    let max_s_idx = s_len - sep_len in
    let rec check_sep i k = match k > max_sep_idx with
    | true ->
        let r_start = i + sep_len in
        Some (String.sub s 0 i, String.sub s r_start (s_len - r_start))
    | false ->
        if unsafe_get s (i + k) = unsafe_get sep k
        then check_sep i (k + 1)
        else scan (i + 1)
    and scan i =
      if i > max_s_idx then None else
      if String.get s i = String.get sep 0 then check_sep i 1 else scan (i + 1)
    in
    scan 0

  let cut_right ~sep s =
    let sep_len = length sep in
    if sep_len = 0 then invalid_arg err_empty_sep else
    let s_len = length s in
    let max_sep_idx = sep_len - 1 in
    let max_s_idx = s_len - 1 in
    let rec check_sep i k = match k > max_sep_idx with
    | true ->
        let r_start = i + sep_len in
        Some (String.sub s 0 i, String.sub s r_start (s_len - r_start))
    | false ->
        if unsafe_get s (i + k) = unsafe_get sep k
        then check_sep i (k + 1)
        else rscan (i - 1)
    and rscan i =
      if i < 0 then None else
      if String.get s i = String.get sep 0 then check_sep i 1 else rscan (i - 1)
    in
    rscan (max_s_idx - max_sep_idx)

  let cuts_left ?(drop_empty = false) ~sep s =
    let rec loop acc s = match cut_left ~sep s with
    | Some (v, vs) -> loop (if drop_empty && v = "" then acc else (v :: acc)) vs
    | None -> List.rev (if drop_empty && s = "" then acc else (s :: acc))
    in
    loop [] s

  let cuts_right ?(drop_empty = false) ~sep s =
    let rec loop acc s = match cut_right ~sep s with
    | Some (vs, v) -> loop (if drop_empty && v = "" then acc else (v :: acc)) vs
    | None -> if drop_empty && s = "" then acc else (s :: acc)
    in
    loop [] s

  (* Traversing *)

  let mapi f s =
    let max = length s - 1 in
    let rec try_no_alloc i = match i > max with
    | true -> s
    | false ->
        let c = String.get s i in
        let cm = f i c in
        match cm = c with
        | true -> try_no_alloc (i + 1)
        | false ->
            let b = Bytes.of_string s in
            Bytes.set b i cm;
            with_buf b (i + 1)
    and with_buf b i = match i > max with
    | true -> Bytes.unsafe_to_string b
    | false -> Bytes.set b i (f i (String.get s i)); with_buf b (i + 1)
    in
    try_no_alloc 0

  let map f s = mapi (fun _ c -> f c) s

  (* Formatting *)

  let pp = Fmt.string
  let pp_dump ppf s = Fmt.pf ppf "%S" s

  (* Suggesting *)

  let edit_distance s0 s1 =
    (* As found here http://rosettacode.org/wiki/Levenshtein_distance#OCaml *)
    let minimum a b c = min a (min b c) in
    let m = String.length s0 in
    let n = String.length s1 in
    (* for all i and j, d.(i).(j) will hold the Levenshtein distance between
       the first i characters of s and the first j characters of t *)
    let d = Array.make_matrix (m+1) (n+1) 0 in
    for i = 0 to m do d.(i).(0) <- i done;
    for j = 0 to n do d.(0).(j) <- j done;
    for j = 1 to n do
      for i = 1 to m do
        if s0.[i-1] = s1.[j-1]
        then d.(i).(j) <- d.(i-1).(j-1)  (* no operation required *)
        else
        d.(i).(j) <- minimum
            (d.(i-1).(j) + 1)   (* a deletion *)
            (d.(i).(j-1) + 1)   (* an insertion *)
            (d.(i-1).(j-1) + 1) (* a substitution *)
      done;
    done;
    d.(m).(n)

  let suggest ?(dist = 2) candidates s =
    let add (min, acc) name =
      let d = edit_distance s name in
      if d = min then min, (name :: acc) else
      if d < min then d, [name] else
      min, acc
    in
    let d, suggs = List.fold_left add (max_int, []) candidates in
    if d <= dist (* suggest only if not too far *) then List.rev suggs else []

  (* Escaping and unescaping bytes

     XXX: limitation cannot escape multiple bytes (i.e. non US-ASCII UTF-8) *)

  let escaped_length char_len s =
    let rec loop s max i l = match i > max with
    | true -> l
    | false -> loop s max (i + 1) (l + char_len s.[i])
    in
    loop s (String.length s - 1) 0 0

  let escaper char_len set_char s =
    let len = String.length s in
    let escaped_len = escaped_length char_len s in
    match escaped_len = len with
    | true -> s
    | false ->
        let b = Bytes.create escaped_len in
        let rec loop s max i k = match i > max with
        | true -> Bytes.unsafe_to_string b
        | false -> loop s max (i + 1) (set_char b k s.[i])
        in
        loop s (len - 1) 0 0

  exception Illegal_escape of int (* index *)

  let unescaped_length char_len_at s =
    let rec loop max i len = match i > max with
    | true -> len
    | false ->
        let esc_len = char_len_at s i in
        loop max (i + esc_len) (len - esc_len + 1)
    in
    loop (String.length s - 1) 0 (String.length s)

  let unescaper char_len_at set_char s =
    try
      let len = String.length s in
      let unescaped_len = unescaped_length char_len_at s in
      match len = unescaped_len with
      | true -> Ok s
      | false ->
          let b = Bytes.create unescaped_len in
          let rec loop s max i k = match i > max with
          | true -> Ok (Bytes.unsafe_to_string b)
          | false -> loop s max (set_char b k s i) (k + 1)
          in
          loop s (String.length s - 1) 0 0
    with
    | Illegal_escape i -> Error i

  (* US-ASCII string support *)

  module Ascii = struct

    (* Predicates *)

    let is_valid s =
      let rec loop max i = match i > max with
      | true -> true
      | false when unsafe_get s i > Char.Ascii.max -> false
      | false -> loop max (i + 1)
      in
      loop (String.length s - 1) 0

    (* Casing transforms *)

    let caseify is_not_case to_case s =
      let max_idx = length s - 1 in
      let caseify b i =
        for k = i to max_idx do
          Bytes.unsafe_set b k (to_case (unsafe_get s k))
        done;
        Bytes.unsafe_to_string b
      in
      let rec try_no_alloc i =
        if i > max_idx then s else
        if is_not_case (unsafe_get s i) then caseify (Bytes.of_string s) i else
        try_no_alloc (i + 1)
      in
      try_no_alloc 0

    let uppercase s = caseify Char.Ascii.is_lower Char.Ascii.uppercase s
    let lowercase s = caseify Char.Ascii.is_upper Char.Ascii.lowercase s

    let caseify_first is_not_case to_case s =
      if length s = 0 then s else
      let c = unsafe_get s 0 in
      if not (is_not_case c) then s else
      let b = Bytes.of_string s in
      Bytes.unsafe_set b 0 (to_case c);
      Bytes.unsafe_to_string b

    let capitalize s = caseify_first Char.Ascii.is_lower Char.Ascii.uppercase s
    let uncapitalize s =
      caseify_first Char.Ascii.is_upper Char.Ascii.lowercase s

    (* Converting to US-ASCII hexadecimal characters *)

    let to_hex s =
      let rec loop max s i h k = match i > max with
      | true -> Bytes.unsafe_to_string h
      | false ->
          let byte = Char.code s.[i] in
          Bytes.set h k (Char.Ascii.lower_hex_digit (byte lsr 4));
          Bytes.set h (k + 1) (Char.Ascii.lower_hex_digit byte);
          loop max s (i + 1) h (k + 2)
      in
      let len = String.length s in
      let h = Bytes.create (2 * len) in
      loop (len - 1) s 0 h 0

    let of_hex h =
      let hex_value s i = match s.[i] with
      | '0' .. '9' as c -> Char.code c - 0x30
      | 'A' .. 'F' as c -> 10 + (Char.code c - 0x41)
      | 'a' .. 'f' as c -> 10 + (Char.code c - 0x61)
      | _ -> raise_notrace (Illegal_escape i)
      in
      match String.length h with
      | len when len mod 2 <> 0 -> Error len
      | len ->
          let rec loop max s i h k = match i > max with
          | true -> Ok (Bytes.unsafe_to_string s)
          | false ->
              let hi = hex_value h k and lo = hex_value h (k + 1) in
              Bytes.set s i (Char.chr @@ (hi lsl 4) lor lo);
              loop max s (i + 1) h (k + 2)
          in
          let s_len = len / 2 in
          let s = Bytes.create s_len in
          try loop (s_len - 1) s 0 h 0 with Illegal_escape i -> Error i

    (* Converting to printable US-ASCII characters *)

    let set_ascii_unicode_escape b k c = (* for c <= 0x7F *)
      let byte = Char.code c in
      let hi = byte lsr 4 and lo = byte land 0xF in
      Bytes.blit_string "\\u{00" 0 b k 5;
      Bytes.set b (k + 5) (Char.Ascii.upper_hex_digit hi);
      Bytes.set b (k + 6) (Char.Ascii.upper_hex_digit lo);
      Bytes.set b (k + 7) '}';
      k + 8

    let set_hex_escape b k c =
      let byte = Char.code c in
      let hi = byte lsr 4 and lo = byte land 0xF in
      Bytes.blit_string "\\x" 0 b k 2;
      Bytes.set b (k + 2) (Char.Ascii.upper_hex_digit hi);
      Bytes.set b (k + 3) (Char.Ascii.upper_hex_digit lo);
      k + 4

    let set_symbol_escape b k symbol =
      Bytes.set b k '\\'; Bytes.set b (k + 1) symbol; k + 2

    let escape =
      let char_len = function
      | '\x20' .. '\x5B' | '\x5D' .. '\x7E' -> 1
      | _ (* hex escape *) -> 4
      in
      let set_char b k = function
      | '\x20' .. '\x5B' | '\x5D' .. '\x7E' as c -> Bytes.set b k c; k + 1
      | c -> set_hex_escape b k c
      in
      escaper char_len set_char

    let unescape =
      let char_len_at s i = match s.[i] <> '\\' with
      | true -> 1
      | false ->
          let max = String.length s - 1 in
          let j = i + 1 in
          if j > max then raise_notrace (Illegal_escape i) else
          if s.[j] <> 'x' then raise_notrace (Illegal_escape i) else
          let j = i + 3 in
          if j > max then raise_notrace (Illegal_escape i) else
          if Char.Ascii.is_hex_digit s.[i + 2] &&
             Char.Ascii.is_hex_digit s.[i + 3]
          then 4
          else raise (Illegal_escape i) (* invalid esc *)
      in
      let set_char b k s i = match s.[i] <> '\\' with
      | true -> Bytes.set b k s.[i]; i + 1
      | false ->
          (* assert (s.[i+1] = 'x') *)
          let hi = Char.Ascii.hex_digit_value s.[i + 2] in
          let lo = Char.Ascii.hex_digit_value s.[i + 3] in
          Bytes.set b k (Char.chr @@ (hi lsl 4) lor lo); i + 4
      in
      unescaper char_len_at set_char

    let ocaml_string_escape =
      let char_len = function
      | '\b' | '\t' | '\n' | '\r' | '"' | '\\' -> 2
      | '\x20' .. '\x7E' -> 1
      | _ (* hex escape *) -> 4
      in
      let set_char b k = function
      | '\b' -> set_symbol_escape b k 'b'
      | '\t' -> set_symbol_escape b k 't'
      | '\n' -> set_symbol_escape b k 'n'
      | '\r' -> set_symbol_escape b k 'r'
      | '"'  -> set_symbol_escape b k '"'
      | '\\' -> set_symbol_escape b k '\\'
      | '\x20' .. '\x7E' as c -> Bytes.set b k c; k + 1
      | c -> set_hex_escape b k c
      in
      escaper char_len set_char

    let ocaml_unescape =
      let char_len_at s i = match s.[i] <> '\\' with
      | true -> 1
      | false ->
          let max = String.length s - 1 in
          let j = i + 1 in
          if j > max then raise_notrace (Illegal_escape i) else
          match s.[j] with
          | 'x' ->
              let j = i + 3 in
              if j > max then raise_notrace (Illegal_escape i) else
              if Char.Ascii.is_hex_digit s.[i + 2] &&
                 Char.Ascii.is_hex_digit s.[i + 3]
              then 4
              else raise_notrace (Illegal_escape i)
          | 'b' | 't' | 'n' | 'r' | ' ' | '"' | '\\' -> 2
          | 'o' ->
              let j = i + 4 in
              if j > max then raise_notrace (Illegal_escape i) else
              let is_octal = function '0' .. '7' -> true | _ -> false in
              if is_octal s.[i + 2] && is_octal s.[i + 3] && is_octal s.[i + 4]
              then 5
              else raise_notrace (Illegal_escape i)
          | c when Char.Ascii.is_digit c ->
              let j = i + 3 in
              if j > max then raise_notrace (Illegal_escape i) else
              if Char.Ascii.is_digit s.[i + 2] && Char.Ascii.is_digit s.[i + 3]
              then 4
              else raise_notrace (Illegal_escape i)
          | _ -> raise_notrace (Illegal_escape i)
      in
      let set_char b k s i =
        if s.[i] <> '\\' then (Bytes.set b k s.[i]; i + 1) else
        match s.[i + 1] with
        | 'x' ->
            let hi = Char.Ascii.hex_digit_value s.[i + 2] in
            let lo = Char.Ascii.hex_digit_value s.[i + 3] in
            Bytes.set b k (Char.chr @@ (hi lsl 4) lor lo); i + 4
        | '\\' -> Bytes.set b k '\\'; i + 2
        | 'b' -> Bytes.set b k '\b'; i + 2
        | 't' -> Bytes.set b k '\t'; i + 2
        | 'n' -> Bytes.set b k '\n'; i + 2
        | 'r' -> Bytes.set b k '\r'; i + 2
        | ' ' -> Bytes.set b k ' '; i + 2
        | '"' -> Bytes.set b k '"'; i + 2
        | 'o' ->
            let o3 = Char.Ascii.hex_digit_value s.[i + 2] in
            let o2 = Char.Ascii.hex_digit_value s.[i + 3] in
            let o1 = Char.Ascii.hex_digit_value s.[i + 4] in
            let byte = o3 * 64 + o2 * 8 + o1 in
            if byte > 255 then raise_notrace (Illegal_escape i) else
            Bytes.set b k (Char.chr byte); i + 5
        | c when Char.Ascii.is_digit c ->
            let d3 = Char.Ascii.hex_digit_value s.[i + 1] in
            let d2 = Char.Ascii.hex_digit_value s.[i + 2] in
            let d1 = Char.Ascii.hex_digit_value s.[i + 3] in
            let byte = d3 * 100 + d2 * 10 + d1 in
            if byte > 255 then raise_notrace (Illegal_escape i) else
            Bytes.set b k (Char.chr byte); i + 4
        | _ -> assert false
      in
      unescaper char_len_at set_char
  end

  (* String map and sets *)

  module Set = struct
    include Set.Make (String)
    let pp ?sep pp_elt = Fmt.iter ?sep iter pp_elt
    let pp_dump ppf ss = Fmt.pf ppf "@[<1>{%a}@]" (pp ~sep:Fmt.sp pp_dump) ss
  end

  module Map = struct
    include Map.Make (String)
    let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty
    let of_list bs = List.fold_left (fun m (k,v) -> add k v m) empty bs

    let add_to_list k v m = match find k m with
    | exception Not_found -> add k [v] m
    | l -> add k (v :: l) m

    let add_to_set
        (type set) (type elt)
        (module S : Stdlib.Set.S with type elt = elt and type t = set)
        k v m = match find k m with
    | exception Not_found -> add k (S.singleton v) m
    | set -> add k (S.add v set) m

    let pp ?sep pp_binding = Fmt.iter_bindings ?sep iter pp_binding
    let pp_dump_str = pp_dump
    let pp_dump pp_v ppf m =
      let pp_binding ppf (k, v) =
        Fmt.pf ppf "@[<1>(@[%a@],@ @[%a@])@]" pp_dump_str k pp_v v
      in
      Fmt.pf ppf "@[<1>{%a}@]" (pp ~sep:Fmt.sp pp_binding) m

    let pp_dump_string_map ppf m = pp_dump pp_dump_str ppf m
  end

  (* Uniqueness *)

  let uniquify ss =
    let rec loop seen acc = function
    | [] -> List.rev acc
    | s :: ss when Set.mem s seen -> loop seen acc ss
    | s :: ss -> loop (Set.add s seen) (s :: acc) ss
    in
    loop Set.empty [] ss

  let unique ~exists n =
    let rec loop i n = match i > 1_000_000_000 with
    | true -> Fmt.error "Could not uniquify %s after 1e9 retries." n
    | false ->
        let r = Fmt.str "%s~%d" n i in
        if exists r then loop (i + 1) n else Ok r
    in
    if exists n then loop 1 n else Ok n

  (* Substituting *)

  let subst_pct_vars ?buf vars s =
    let max = String.length s - 1 in
    let buf = match buf with
    | None -> Buffer.create (max + 1)
    | Some buf -> Buffer.clear buf; buf
    in
    let add buf s ~start ~last =
      Buffer.add_substring buf s start (last - start + 1)
    in
    let rec find_var_end s i max = match i > max with
    | true -> None
    | false ->
        if i + 1 > max then None else
        if s.[i] = '%' && s.[i + 1] = '%' then Some (i + 1) else
        find_var_end s (i + 1) max
    in
    let rec loop buf s start i max = match i > max with
    | true ->
        if start = 0 then None else
        if start > max then Some (Buffer.contents buf) else
        (add buf s ~start ~last:max; Some (Buffer.contents buf))
    | false ->
        if i + 4 > max then loop buf s start (max + 1) max else
        if s.[i] <> '%' then loop buf s start (i + 1) max else
        if s.[i + 1] <> '%' then loop buf s start (i + 2) max else
        match find_var_end s (i + 3) max with
        | None -> loop buf s start (max + 1) max
        | Some k ->
            let var = with_index_range ~first:(i + 2) ~last:(k - 2) s in
            match Map.find var vars with
            | exception Not_found -> loop buf s start (k + 1) max
            | v ->
                add buf s ~start ~last:(i - 1);
                Buffer.add_string buf v;
                loop buf s (k + 1) (k + 1) max
    in
    loop buf s 0 0 max
end

module List = struct
  include List

  let classify
      (type a) (type b)
      ?(cmp_elts : a -> a -> int = Pervasives.compare)
      ?(cmp_classes : b -> b -> int = Pervasives.compare)
      ~classes:(classes : (a -> b list)) els
    =
    let module S = Set.Make (struct type t = a let compare = cmp_elts end) in
    let module M = Map.Make (struct type t = b let compare = cmp_classes end) in
    let add_classes acc p =
      let add_class acc c = try M.add c (S.add p (M.find c acc)) acc with
      | Not_found -> M.add c (S.singleton p) acc
      in
      List.fold_left add_class acc (classes p)
    in
    let classes = List.fold_left add_classes M.empty els in
    List.rev (M.fold (fun c els acc -> (c, S.elements els) :: acc) classes [])
end

(* File paths *)

module Fpath = struct

  (* Errors *)

  let err_invalid_seg s = Fmt.str "%a: Invalid path segment" String.pp_dump s
  let err_start s = Fmt.error "%a: Not a path" String.pp_dump s
  let err_null s = Fmt.error "%a: Not a path: has null bytes" String.pp_dump s
  let err_empty s = Fmt.error "%a: Not a path: is empty" String.pp_dump s

  (* Pct encoding *)

  let pct_esc_len ~escape_space = function
  | '%' | '#' | '?' -> 3
  | ' ' when escape_space -> 3
  | c when Char.Ascii.is_control c -> 3
  | _ -> 1

  let set_pct_encoded b i c =
    let c = Char.code c in
    let hi = Char.Ascii.upper_hex_digit ((c lsr 4) land 0xF) in
    let lo = Char.Ascii.upper_hex_digit (c land 0xF) in
    Bytes.set b i '%'; Bytes.set b (i + 1) hi; Bytes.set b (i + 2) lo;
    i + 3

  let pct_esc_set_char ~escape_space b i = function
  | '%' | '#' | '?' as c -> set_pct_encoded b i c
  | ' ' as c when escape_space -> set_pct_encoded b i c
  | c when Char.Ascii.is_control c -> set_pct_encoded b i c
  | c -> Bytes.set b i c; i + 1

  (* Platform specifics. *)

  let undouble_sep sep dbl_sep s =
    let rec loop last_is_sep b k s i max = match i > max with
    | true -> Bytes.unsafe_to_string b
    | false ->
        let c = String.get s i in
        let c_is_sep = Char.equal c sep && i <> 0 (* handle // *) in
        let is_dbl = last_is_sep && c_is_sep in
        match is_dbl with
        | true -> loop c_is_sep b k s (i + 1) max
        | false -> Bytes.set b k c; loop c_is_sep b (k + 1) s (i + 1) max
    in
    let len = String.length s in
    loop false (Bytes.create (len - dbl_sep)) 0 s 0 (len - 1)

  module Windows = struct

    (* XXX the {of_string,path_start} needs reviewing/testing *)

    let dir_sep_char = '\\'
    let char_is_dir_sep c = c = '\\' || c = '/'
    let dir_sep = "\\"
    let is_seg s =
      let valid c = c <> dir_sep_char && c <> '/' && c <> '\x00' in
      String.for_all valid s

    let is_unc_path p = String.is_prefix "\\\\" p
    let has_drive p = String.exists (Char.equal ':') p
    let non_unc_path_start p = match String.rindex p ':' with
    | exception Not_found -> 0
    | i -> i + 1 (* exists by construction once injected *)

    let path_start p = (* once [p] is injected this does not raise *)
      match is_unc_path p with
      | false -> non_unc_path_start p
      | true ->
          let plen = String.length p in
          if plen = 2 then raise Not_found else
          let sep_from p from = String.index_from p from dir_sep_char in
          let i = sep_from p 2 in
          let j = sep_from p (i + 1) in
          match p.[i - 1] with
          | '.' when i = 3 -> j
          | '?' when i = 3 ->
              if p.[j - 1] = ':' then j else
              if i + 3 < plen
              && p.[i + 1] = 'U' && p.[i + 2] = 'N' && p.[i + 3] = 'C'
              then sep_from p (sep_from p (j + 1) + 1)
              else sep_from p (j + 1)
          | _ -> sep_from p j

    let last_non_empty_seg_start p = match String.rindex p dir_sep_char with
    | exception Not_found -> path_start p
    | k ->
        match k = String.length p - 1 with
        | false -> k + 1
        | true ->
            match String.rindex_from p (k - 1) dir_sep_char with
            | exception Not_found -> path_start p
            | k -> k + 1

    let chop_volume p = String.with_index_range ~first:(path_start p) p

    let backslashify s =
      let b = Bytes.copy (Bytes.unsafe_of_string s) in
      for i = 0 to Bytes.length b - 1 do
        if Bytes.get b i = '/' then Bytes.set b i '\\'
      done;
      Bytes.unsafe_to_string b

    let of_string s =
      if s = "" then err_empty s else
      try
        let p =
          let rec loop has_slash last_is_sep dbl_sep i max =
            match i > max with
            | true ->
                let s = if has_slash then backslashify s else s in
                if dbl_sep > 0 then undouble_sep dir_sep_char dbl_sep s else s
            | false ->
                let c = String.unsafe_get s i in
                if Char.equal c '\x00' then raise Exit else
                let is_slash = Char.equal c '/' in
                let has_slash = has_slash || is_slash in
                let c_is_sep = (is_slash || Char.equal c dir_sep_char) && i <> 0
                in
                let is_dbl = last_is_sep && c_is_sep in
                let dbl_sep = if is_dbl then dbl_sep + 1 else dbl_sep in
                loop has_slash c_is_sep dbl_sep (i + 1) max
          in
          loop false false 0 0 (String.length s - 1)
        in
        match path_start p with
        | exception Not_found -> err_start p
        | n ->
            let p = match n = String.length p with
            | true -> (* add root if there's only a UNC volume *) p ^ dir_sep
            | false -> p
            in
            Ok p
      with Exit -> err_null s

    let append p0 p1 =
      match is_unc_path p1 || has_drive p1 || p1.[0] = dir_sep_char with
      | true (* with volume or absolute *) -> p1
      | false ->
          let p0_last_is_sep = p0.[String.length p0 - 1] = dir_sep_char in
          let sep = if p0_last_is_sep then "" else dir_sep in
          String.concat sep [p0; p1]

    let is_rel p = match is_unc_path p with
    | true -> false
    | false -> p.[non_unc_path_start p] <> dir_sep_char

    let is_root p = p.[path_start p] = dir_sep_char

    let to_uri_path ?(escape_space = true) p =
      let set_char b i = function
      | '\\' -> Bytes.set b i '/'; i + 1
      | c -> pct_esc_set_char ~escape_space b i c
      in
      String.escaper (pct_esc_len ~escape_space) set_char p
  end

  module Posix = struct
    let dir_sep_char = '/'
    let char_is_dir_sep c = Char.equal c '/'
    let dir_sep = "/"
    let is_seg s = String.for_all (fun c -> c <> dir_sep_char && c <> '\x00') s
    let of_string = function
    | "" as s -> err_empty s
    | s ->
        try
          let rec loop last_is_sep dbl_sep i max = match i > max with
          | true ->
              if dbl_sep > 0 then Ok (undouble_sep dir_sep_char dbl_sep s) else
              Ok s
          | false ->
              let c = String.unsafe_get s i in
              if Char.equal c '\x00' then raise Exit else
              let c_is_sep = Char.equal c dir_sep_char && i <> 0 in
              let is_dbl = last_is_sep && c_is_sep in
              let dbl_sep = if is_dbl then dbl_sep + 1 else dbl_sep in
              loop c_is_sep dbl_sep (i + 1) max
          in
          loop false 0 0 (String.length s - 1)
        with
        | Exit -> err_null s

    let last_non_empty_seg_start p = match String.rindex p dir_sep_char with
    | exception Not_found -> 0
    | k ->
        match k = String.length p - 1 with
        | false -> k + 1
        | true ->
            match String.rindex_from p (k - 1) dir_sep_char with
            | exception Not_found -> 0
            | k -> k + 1

    let dir_sep_char = '/'
    let last_non_empty_seg_start p = match String.rindex p dir_sep_char with
    | exception Not_found -> 0
    | k ->
        match k = String.length p - 1 with
        | false -> k + 1
        | true ->
            match String.rindex_from p (k - 1) dir_sep_char with
            | exception Not_found -> 0
            | k -> k + 1

    let path_start p = 0
    let chop_volume p = p
    let append p0 p1 =
      if p1.[0] = dir_sep_char (* absolute *) then p1 else
      let p0_last_is_sep = p0.[String.length p0 - 1] = dir_sep_char in
      let sep = if p0_last_is_sep then "" else dir_sep in
      String.concat sep [p0; p1]

    let is_rel p = p.[0] <> dir_sep_char
    let is_root p = String.equal p dir_sep || String.equal p "//"

    let to_uri_path ?(escape_space = true) p =
      String.escaper (pct_esc_len ~escape_space)
        (pct_esc_set_char ~escape_space) p
  end

  let path_start = if Sys.win32 then Windows.path_start else Posix.path_start
  let chop_volume = if Sys.win32 then Windows.chop_volume else Posix.chop_volume

  (* Separators and segments *)

  let dir_sep_char =
    if Sys.win32 then Windows.dir_sep_char else Posix.dir_sep_char

  let dir_sep = if Sys.win32 then Windows.dir_sep else Posix.dir_sep
  let char_is_dir_sep =
    if Sys.win32 then Windows.char_is_dir_sep else Posix.char_is_dir_sep

  let last_is_dir_sep p = Char.equal (p.[String.length p - 1]) dir_sep_char

  let is_seg = if Sys.win32 then Windows.is_seg else Posix.is_seg
  let is_rel_seg = function "." | ".." -> true | _ -> false

  let last_seg_len p = match String.rindex p dir_sep_char with
  | exception Not_found -> String.length p
  | k -> String.length p - (k + 1)

  let last_non_empty_seg_start = match Sys.win32 with
  | true -> Windows.last_non_empty_seg_start
  | false -> Posix.last_non_empty_seg_start

  (* Paths *)

  type t = string (* N.B. a path is never "" *)
  let of_string = if Sys.win32 then Windows.of_string else Posix.of_string
  let to_string p = p
  let v s = match of_string s with Ok p -> p | Error m -> invalid_arg m
  let add_seg p seg =
    if not (is_seg seg) then invalid_arg (err_invalid_seg seg) else
    let sep = if last_is_dir_sep p then "" else dir_sep in
    String.concat sep [p; seg]

  let append = if Sys.win32 then Windows.append else Posix.append

  (* Directory paths *)

  let is_dir_path p = (* check is . .. or ends with / /. or /.. *)
    let k = String.length p - 1 in
    if k < 0 then (* should not happen *) false else
    match p.[k] with
    | c when Char.equal c dir_sep_char -> true
    | '.' ->
        let k = k - 1 in
        if k < 0 then true else
        begin match p.[k] with
        | c when Char.equal c dir_sep_char -> true
        | '.' ->
            let k = k - 1 in
            k < 0 || Char.equal p.[k] dir_sep_char
        | _ -> false
        end
    | _ -> false

  let to_dir_path p = add_seg p ""

  (* Strict prefixes *)

  let is_prefix pre p = match String.is_prefix pre p with
  | false -> false
  | true ->
      let suff_start = String.length pre in
      let p_len = String.length p in
      (* Check [prefix] and [p] are not equal modulo directoryness. *)
      if suff_start = p_len then false else
      if suff_start = p_len - 1 && p.[suff_start] = dir_sep_char then false else
      (* Check the prefix is segment based *)
      (pre.[suff_start - 1] = dir_sep_char || p.[suff_start] = dir_sep_char)

  let rem_prefix pre p = match is_prefix pre p with
  | false -> None
  | true ->
      let len = String.length pre in
      let first = if p.[len] = dir_sep_char then len + 1 else len in
      Some (String.with_index_range p ~first)

  let reroot ~root ~dst src =
    let rel_file = Option.get (rem_prefix root src) in
    append dst rel_file

  (* Predicates and comparisons *)

  let is_rel = if Sys.win32 then Windows.is_rel else Posix.is_rel
  let is_abs p = not (is_rel p)
  let is_root = if Sys.win32 then Windows.is_root else Posix.is_root

  (* FIXME this is wrong on windows. *)
  let current_dir_dir = "." ^ dir_sep
  let is_current_dir p = String.equal p "." || String.equal p current_dir_dir
  let parent_dir_dir = ".." ^ dir_sep
  let is_parent_dir p = String.equal p ".." || String.equal p parent_dir_dir

  let equal = String.equal
  let compare = String.compare

  (* File extensions *)

  type ext = string
  let ext_sep_char = '.'

  let rec ext_single_range spos epos k p =
    let i = String.rindex_from p k ext_sep_char (* raises if not fnd *) in
    match i <= spos with
    | true -> raise Not_found
    | false ->
        match not (Char.equal p.[i - 1] ext_sep_char) with
        | true -> i, epos
        | false -> ext_single_range spos epos (i - 1) p

  let rec ext_multi_range epos k p =
    let i = String.index_from p k ext_sep_char (* raises if not fnd *) in
    match i > epos with
    | true -> raise Not_found
    | false ->
        match not (Char.equal p.[i - 1] ext_sep_char) with
        | true -> i, epos
        | false -> ext_multi_range epos (i + 1) p

  let ext_range ?(multi = false) p =
    let plen = String.length p in
    let seg_start = last_non_empty_seg_start p in
    let seg_stop = match last_is_dir_sep p with
    | true -> plen - 2
    | false -> plen - 1
    in
    if seg_start >= seg_stop then raise Not_found else
    match multi with
    | true -> ext_multi_range seg_stop (seg_start + 1) p
    | false -> ext_single_range seg_start seg_stop seg_stop p

  let get_ext ?multi p = match ext_range ?multi p with
  | exception Not_found -> ""
  | first, last -> String.with_index_range ~first ~last p

  let has_ext e p = match ext_range ~multi:true p with
  | exception Not_found -> String.equal e ""
  | first, last ->
      let plen = last - first + 1 in
      let elen = String.length e in
      match plen < elen with
      | true -> false
      | false ->
          let rec loop pi ei = match ei < 0 with
          | true -> true
          | false -> Char.equal p.[pi] e.[ei] && loop (pi - 1) (ei - 1)
          in
          loop last (elen - 1)

  let mem_ext exts p = List.exists (fun ext -> has_ext ext p) exts

  let add_ext e p =
    let plen = String.length p - 1 in
    match last_is_dir_sep p with
    | false -> p ^ e
    | true ->
        let elen = String.length e in
        let nlen = plen + elen in
        let n = Bytes.create nlen in
        Bytes.blit_string p 0 n 0 (plen - 1);
        Bytes.blit_string e 0 n (plen - 1) elen;
        Bytes.set n (nlen - 1) dir_sep_char;
        Bytes.unsafe_to_string n

  let _rem_ext efirst elast p =
    let plen = String.length p in
    match elast = plen - 1 with
    | true -> String.with_index_range ~last:(efirst - 1) p
    | false ->
        let elen = elast - efirst + 1 in
        let nlen = plen - elen in
        let n = Bytes.create nlen in
        Bytes.blit_string p 0 n 0 nlen;
        Bytes.set n (nlen - 1) dir_sep_char;
        Bytes.unsafe_to_string n

  let rem_ext ?multi p = match ext_range ?multi p with
  | exception Not_found -> p
  | efirst, elast -> _rem_ext efirst elast p

  let set_ext ?multi e p = add_ext e (rem_ext ?multi p)

  let cut_ext ?multi p = match ext_range ?multi p with
  | exception Not_found -> p, ""
  | efirst, elast ->
      let ext = String.with_index_range ~first:efirst ~last:elast p in
      let p = _rem_ext efirst elast p in
      p, ext

  (* Basename and parent directory *)

  let basename ?(no_ext = false) p =
    let max = String.length p - 1 in
    let first, last = match String.rindex p dir_sep_char with
    | exception Not_found -> (* B *) path_start p, max
    | k when k <> max || k = 0 -> (* /B or .../B *) k + 1, max
    | k -> (* .../ *)
        let j = k - 1 in
        match String.rindex_from p j dir_sep_char with
        | exception Not_found -> (* B/ *) path_start p, j
        | i -> (* .../B/ *) i + 1, j
    in
    match last - first + 1 with
    | 1 when p.[first] = '.' -> ""
    | 2 when p.[first] = '.' && p.[first + 1] = '.' -> ""
    | _ when not no_ext -> String.with_index_range ~first ~last p
    | _ -> (* Drop multi ext *)
        let rec loop first last i = match i > last with
        | true -> String.with_index_range ~first ~last p
        | false ->
            match p.[i] = ext_sep_char with
            | false -> loop first last (i + 1)
            | true ->
                if p.[i - 1] = ext_sep_char then loop first last (i + 1) else
                String.with_index_range ~first ~last:(i - 1) p
        in
        loop first last (first + 1)

  let rec parent p =
    let plen = String.length p in
    let seg_start = last_non_empty_seg_start p in
    let seg_stop = match last_is_dir_sep p with
    | true -> plen - 2
    | false -> plen - 1
    in
    let seg_len = seg_stop - seg_start + 1 in
    let via_dotdot p = add_seg (add_seg p "..") "" in
    match seg_len with
    | 0 -> p
    | 1 when p.[seg_start] = '.' ->
        if seg_start = 0 then "../" else
        parent (String.with_index_range ~last:(seg_start - 1) p)
    | 2 when p.[seg_start] = '.' && p.[seg_stop] = '.' -> via_dotdot p
    | _ when seg_start = 0 -> "./"
    | _ -> add_seg (String.with_index_range ~last:(seg_start - 1) p) ""

  let equal_basename p0 p1 = (* XXX could avoid alloc *)
    String.equal (basename p0) (basename p1)

  (* Converting *)

  let to_uri_path = if Sys.win32 then Windows.to_uri_path else Posix.to_uri_path
  let pp_quoted ppf p = String.pp ppf (Filename.quote p)
  let pp_unquoted = String.pp
  let pp = pp_quoted
  let pp_dump = String.pp_dump

  (* Uniqueness *)

  let uniquify = String.uniquify

  (* Path and sets *)

  type path = t
  module Set = struct
    let pp_set ppf ss =
      Fmt.pf ppf "@[<1>{%a}@]" (String.Set.pp ~sep:Fmt.sp pp) ss

    include String.Set
  end
  module Map = String.Map

  (* Sorts *)

  let sort_by_parent ps =
    let add_path p acc = Map.add_to_set (module Set) (parent p) p acc in
    Set.fold add_path ps Map.empty

  let sort_by_ext ?multi ps =
    let add_path p acc =
      String.Map.add_to_set (module Set) (get_ext ?multi p) p acc
    in
    Set.fold add_path ps String.Map.empty

  (* Search paths *)

  let search_path_sep = if Sys.win32 then ";" else ":"
  let list_of_search_path ?(sep = search_path_sep) path =
    let rec loop acc = function
    | ""  -> Ok (List.rev acc)
    | p ->
        let dir, p = match String.cut_left ~sep p with
        | None -> p, ""
        | Some (dir, p) -> dir, p
        in
        if dir = "" then loop acc p else
        match of_string dir with
        | Error e -> Fmt.error "search path %s: %S: %s" path dir e
        | Ok d -> loop (d :: acc) p
    in
    loop [] path

  (* Operators *)

  let ( / ) = add_seg
  let ( // ) = append
  let ( + ) p e = add_ext e p
  let ( -+ ) p e = set_ext e p
end

(* Hash values and functions *)

module Hash = struct

  (* Hash values *)

  type t = string
  let nil = ""
  let length = String.length

  (* Predicates and comparisons *)

  let equal = String.equal
  let compare = String.compare
  let is_nil h = equal nil h

  (* Converting *)

  let to_bytes h = h
  let of_bytes h = h
  let to_hex = String.Ascii.to_hex
  let of_hex = String.Ascii.of_hex
  let pp ppf h = Fmt.string ppf (if is_nil h then "nil" else to_hex h)

  (* Hash functions *)

  module type T = sig
    val id : string
    val length : int
    val string : string -> t
    val fd : Unix.file_descr -> t
    val file : Fpath.t -> (t, string) result
  end

  let rec file_with_hash_fd hash_fd f =
    let err f e = Fmt.error "%a: %s" Fpath.pp f e in
    match Unix.openfile (Fpath.to_string f) Unix.[O_RDONLY] 0 with
    | exception Unix.Unix_error (Unix.EINTR, _, _) ->
        file_with_hash_fd hash_fd f
    | exception Unix.Unix_error (e, _, _) -> err f (Unix.error_message e)
    | fd ->
        match hash_fd fd with
        | exception Sys_error e ->
            (try Unix.close fd with Unix.Unix_error (_, _, _) -> ()); err f e
        | hash ->
            match Unix.close fd with
            | () -> Ok hash
            | exception Unix.Unix_error (e, _, _)  ->
                err f (Unix.error_message e)

  module Murmur3_128 = struct
    type t = string
    type seed = int
    let no_seed = 0
    external hash_fd : Unix.file_descr -> seed -> t = "ocaml_b0_murmurhash_fd"
    external hash_unsafe : string -> int -> int -> seed -> t =
      "ocaml_b0_murmurhash"

    let id = "murmur3-128"
    let length = 16
    let string s = hash_unsafe s 0 (String.length s) no_seed
    let fd fd = hash_fd fd no_seed
    let file f = file_with_hash_fd fd f
  end

  module Xxh_64 = struct
    type t = int64
    type seed = int64
    external hash_fd : Unix.file_descr -> seed -> t = "ocaml_b0_xxhash_fd"
    external hash_unsafe : string -> int -> int -> seed -> t = "ocaml_b0_xxhash"
    external set_64u : Bytes.t -> int -> int64 -> unit = "%caml_string_set64u"
    external swap_64 : int64 -> int64 = "%bswap_int64"
    external noswap : int64 -> int64 = "%identity"
    let layout = if Sys.big_endian then noswap else swap_64
    let to_bytes t =
      let b = Bytes.create 8 in
      set_64u b 0 (layout t); Bytes.unsafe_to_string b

    let id = "xxh64"
    let seed = 0L
    let length = 8
    let string s = hash_unsafe s 0 (String.length s) seed |> to_bytes
    let fd fd = hash_fd fd seed |> to_bytes
    let file f = file_with_hash_fd fd f
  end

  let funs = ref [(module Murmur3_128 : T); (module Xxh_64 : T);]
  let add_fun m = funs := m :: !funs
  let funs () = !funs
end

(* Measuring time *)

module Time = struct
  type uint64 = int64
  let uint64_compare a b = Int64.(compare (sub a min_int) (sub b min_int))

  (* Time spans

     Represented by a nanosecond magnitude stored in an unsigned 64-bit
     integer. Allows to represent spans for ~584.5 Julian years. *)

  type span = uint64
  module Span = struct

    (* Time spans *)

    type t = span
    let zero = 0L
    let one = 1L
    let max = -1L
    let add = Int64.add
    let abs_diff s0 s1 = match uint64_compare s0 s1 < 0 with
    | true ->  Int64.sub s1 s0
    | false -> Int64.sub s0 s1

    (* Predicates and comparisons *)

    let equal = Int64.equal
    let compare = uint64_compare

    (* Conversions *)

    let to_uint64_ns s = s
    let of_uint64_ns ns = ns
    let pp = Fmt.uint64_ns_span
    let pp_ns ppf s = Fmt.pf ppf "%Lu" s
  end

  (* Monotonic time counter *)

  type counter = uint64
  external now_ns : unit -> uint64 = "ocaml_b0_monotonic_now_ns"
  let counter = now_ns
  let count c = Int64.sub (now_ns ()) c

  (* CPU time spans *)

  type cpu_span = Unix.process_times
  let cpu_zero =
    Unix.{ tms_utime = 0.; tms_stime = 0.; tms_cutime = 0.; tms_cstime = 0. }

  let sec_to_span sec = Int64.of_float (sec *. 1e9)
  let cpu_utime c = sec_to_span c.Unix.tms_utime
  let cpu_stime c = sec_to_span c.Unix.tms_stime
  let cpu_children_utime c = sec_to_span c.Unix.tms_cutime
  let cpu_children_stime c = sec_to_span c.Unix.tms_cstime

  (* CPU counters *)

  type cpu_counter = cpu_span
  let cpu_counter () = Unix.times ()
  let cpu_count c =
    let now = Unix.times () in
    Unix.{ tms_utime = now.tms_utime -. c.tms_utime;
           tms_stime = now.tms_stime -. c.tms_stime;
           tms_cutime = now.tms_cutime -. c.tms_cutime;
           tms_cstime = now.tms_cstime -. c.tms_cstime; }
end

(* Command lines *)

module Cmd = struct

  (* Command lines *)

  type t =
  | A of string
  | Shield of t
  | Rseq of t list (* Sequence is reversed; only empty at toplevel *)

  let empty = Rseq []
  let rec is_empty = function
  | Rseq [] -> true
  | _ -> false

  let arg a = A a

  let append l0 l1 = match l0, l1 with
  | Rseq [], l1 -> l1
  | l0, Rseq [] -> l0
  | Rseq ls, l  -> Rseq (l :: ls)
  | l1, l2 -> Rseq ([l2; l1])

  let shield = function
  | Rseq [] -> empty
  | l -> Shield l

  let ( % ) l a = append l (arg a)
  let ( %% ) = append

  (* Derived combinators *)

  let if' cond l = if cond then l else empty
  let path p = A (Fpath.to_string p)
  let spath p = Shield (A (Fpath.to_string p))

  let args ?slip l = match slip with
  | None -> Rseq (List.rev_map arg l)
  | Some slip -> Rseq (List.fold_left (fun acc v -> A v :: A slip :: acc) [] l)

  let rev_args ?slip l = match slip with
  | None -> Rseq (List.map arg l)
  | Some slip -> Rseq (List.fold_right (fun v acc -> A v :: A slip :: acc) l [])

  let of_list ?slip conv l = match slip with
  | None -> Rseq (List.rev_map (fun a -> A (conv a)) l)
  | Some slip ->
      let add acc v = A (conv v) :: A slip :: acc in
      Rseq (List.fold_left add [] l)

  let of_rev_list ?slip conv l = match slip with
  | None -> Rseq (List.rev_map (fun a -> A (conv a)) l)
  | Some slip ->
      let add a acc = A (conv a) :: A slip :: acc in
      Rseq (List.fold_right add l [])

  let paths ?slip ps = of_list ?slip Fpath.to_string ps
  let rev_paths ?slip ps = of_rev_list ?slip Fpath.to_string ps

  (* Converting *)

  let to_list l =
    let rec loop acc = function
    | A a -> a :: acc
    | Rseq ls -> List.fold_left loop acc ls
    | Shield l -> loop acc l
    in
    loop [] l

  let to_list_and_stamp l =
    let rec loop shielded acc sg = function
    | A a -> (a :: acc), (if shielded then sg else a :: sg)
    | Rseq ls ->
        let rec sub shielded acc sg = function
        | [] -> acc, sg
        | l :: ls ->
            let acc, sg = loop shielded acc sg l in
            sub shielded acc sg ls
        in
        sub shielded acc sg ls
    | Shield l -> loop true acc sg l
    in
    loop false [] [] l

  let to_stamp l =
    let rec loop acc = function
    | A a -> (a :: acc)
    | Rseq ls ->  List.fold_left loop acc ls
    | Shield l -> acc
    in
    loop [] l

  let of_string s =
  (* Parsing is loosely based on
     http://pubs.opengroup.org/onlinepubs/009695399/utilities/\
     xcu_chap02.html#tag_02_03

     XXX Rewrite, this was quickly ported from bos code based on
     Astring.String.sub *)
    try
      let err_unclosed kind _ =
        Fmt.failwith "unclosed %s quote delimited string" kind
      in
      let skip_white s = String.lose_left Char.Ascii.is_white s in
      let tok_sep c = c = '\'' || c = '\"' || Char.Ascii.is_white c in
      let tok_char c = not (tok_sep c) in
      let not_squote c = c <> '\'' in
      let tail s = (* Yikes *) String.with_index_range ~first:1 s in
      let parse_squoted s =
        let tok, rem = String.span_left not_squote (tail s) in
        if not (String.equal rem "") then tok, tail rem else
        err_unclosed "single" s
      in
      let parse_dquoted acc s =
      let is_data = function '\\' | '"' -> false | _ -> true in
      let rec loop acc s =
        let data, rem = String.span_left is_data s in
        match String.head rem with
        | Some '"' -> (data :: acc), (tail rem)
        | Some '\\' ->
            let rem = tail rem in
            begin match String.head rem with
            | Some ('"' | '\\' | '$' | '`' as c) ->
                let acc = String.(of_char c) :: data :: acc in
                loop acc (tail rem)
            | Some ('\n') -> loop (data :: acc) (tail rem)
            | Some c ->
                let acc = Pervasives.(^) data (Fmt.str "\\%c" c) :: acc in
                loop acc (tail rem)
            | None ->
                err_unclosed "double" s
            end
        | None -> err_unclosed "double" s
        | Some _ -> assert false
      in
      loop acc (tail s)
      in
      let parse_token s =
        let ret acc s = String.concat "" (List.rev acc), s in
        let rec loop acc s = match String.head s with
        | None -> ret acc s
        | Some c when Char.Ascii.is_white c -> ret acc s
        | Some '\'' ->
            let tok, rem = parse_squoted s in loop (tok :: acc) rem
        | Some '\"' ->
            let acc, rem = parse_dquoted acc s in loop acc rem
        | Some c ->
            let sat = tok_char in
            let tok, rem = String.span_left sat s in loop (tok :: acc) rem
        in
        loop [] s
      in
      let rec loop acc s = match String.equal s "" with
      | false ->
          let token, s = parse_token s in
          loop (A token :: acc) (skip_white s)
      | true ->
          match acc with
          | [a] -> a
          | acc -> Rseq acc
      in
      Ok (loop [] (skip_white s))
    with Failure err -> Fmt.error "command line %a: %s" String.pp_dump s err

  let to_string l = String.concat " " (List.map Filename.quote @@ to_list l)
  let pp ppf l = Fmt.pf ppf "@[%a@]" Fmt.(list ~sep:sp string) (to_list l)
  let pp_dump ppf l =
    let pp_arg ppf a = Fmt.string ppf (Filename.quote a) in
    Fmt.pf ppf "@[<h>%a@]" Fmt.(list ~sep:sp pp_arg) (to_list l)

  let rec fold ~arg ~shield ~append ~empty = function
  | A a -> arg a
  | Shield c -> shield (fold ~arg ~shield ~append ~empty c)
  | Rseq l ->
      let append acc v = append (fold ~arg ~shield ~append ~empty v) acc in
      List.fold_left append empty l

  let rec iter_enc ~arg ~shield ~append ~empty e = function
  | A a -> arg e a
  | Shield c -> shield e; iter_enc ~arg ~shield ~append ~empty e c
  | Rseq l ->
      let append e v = append e; iter_enc ~arg ~shield ~append ~empty e v; e in
      ignore (List.fold_left append e l); empty e

  (* Tools *)

  type tool = Fpath.t

  let rec tool = function
  | A a -> Result.to_option (Fpath.of_string a)
  | Shield l -> tool l
  | Rseq ls ->
      let rec loop = function
      | [l] -> tool l
      | l :: ls -> loop ls
      | [] -> None
      in
      loop ls

  let rec set_tool tool = function
  | Rseq [] -> None
  | l ->
      let rec loop = function
      | A a -> A (Fpath.to_string tool)
      | Shield l -> Shield (loop l)
      | Rseq ls ->
          match List.rev ls with
          | arg :: args -> Rseq (List.rev @@ (loop arg) :: args)
          | [] -> assert false
      in
      Some (loop l)

  let get_tool l = match tool l with
  | Some t -> t
  | None when is_empty l -> invalid_arg "empty command line"
  | None -> Fmt.invalid_arg "cmd %s: tool parse error" (to_string l)

  let pp_tool ppf t =
    Fmt.tty_string [`Fg `Blue] ppf (Filename.quote (Fpath.to_string t))

  (* Predicates *)

  let rec is_singleton = function
  | A a -> true
  | Shield l -> is_singleton l
  | Rseq _ -> false
end

(* Operating system interactions *)

module Os = struct

  (* A bit of randomness for functions that need unique filenames *)

  let rand_gen = lazy (Random.State.make_self_init ())

  (* Error handling *)

  let uerr = Unix.error_message
  let err_doing doing e = Fmt.str "%s: %s" doing e
  let ferr file e = Fmt.error "%a: %s" Fpath.pp file e
  let ffail file e = Fmt.failwith "%a: %s" Fpath.pp file e
  let ffail_notrace file e = Fmt.failwith_notrace "%a: %s" Fpath.pp file e

  module Env = struct
    type t = string String.Map.t
    let empty = String.Map.empty
    let override env ~by =
      if String.Map.is_empty by then env else
      let lean_right _ l r = match r with
      | Some _ as v -> v
      | None -> match l with Some _ as v -> v | None -> assert false
      in
      String.Map.merge lean_right env by

    let env_err e = Fmt.error "process environment: %s" e
    let find ~empty_to_none name = match Unix.getenv name with
    | "" when empty_to_none -> None
    | v -> Some v
    | exception Not_found -> None

    let find_value parse ~empty_to_none name =
      match find ~empty_to_none name with
      | None -> None
      | Some v ->
          match parse v with
          | Error e -> Some (Fmt.error "%s environment variable: %s" name e)
          | Ok v -> Some (Ok v)

    type assignments = string list
    let current_assignments () =
      try Ok (Array.to_list @@ Unix.environment ()) with
      | Sys_error e -> env_err e
      | Unix.Unix_error (e, _, _) -> env_err (uerr e)

    let parse_assignments ?(init = String.Map.empty) fold v =
      try
        let add acc assign = match String.cut_left ~sep:"=" assign with
        | Some (var, value) -> String.Map.add var value acc
        | None ->
            Fmt.failwith_notrace "%S: cannot parse VAR=VAL assignement" assign
        in
        Ok (fold add init v)
      with
      | Failure e -> Result.error e

    let of_assignments ?init l = parse_assignments ?init List.fold_left l
    let to_assignments env =
      let add var v acc = String.concat "=" [var; v] :: acc in
      String.Map.fold add env []

    let current () =
      match parse_assignments Array.fold_left (Unix.environment ()) with
      | Ok _ as v -> v
      | Error e -> env_err e
      | exception Sys_error e -> env_err e
      | exception Unix.Unix_error (e, _, _) -> env_err (uerr e)
  end

  module Fd = struct
    let unix_buffer_size = 65536 (* UNIX_BUFFER_SIZE 4.0.0 *)

    let rec openfile fn mode perm = try Unix.openfile fn mode perm with
    | Unix.Unix_error (Unix.EINTR, _, _) -> openfile fn mode perm

    let rec close fd = try Unix.close fd with
    | Unix.Unix_error (Unix.EINTR, _, _) -> close fd

    let close_no_unix_exn fd = try close fd with Unix.Unix_error _ -> ()

    let apply ~close fd f =
      let close fd = try close fd with Unix.Unix_error _ -> () in
      match f fd with v -> close fd; v | exception e -> close fd; raise e

    let copy ?buf ~src dst =
      let rec unix_read fd b = try Unix.read fd b 0 (Bytes.length b) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd b
      in
      let rec unix_write fd s i l =
        let rec write fd s i l = try Unix.single_write fd s i l with
        | Unix.Unix_error (Unix.EINTR, _, _) -> write fd s i l
        in
        let bc = write fd s i l in
        if bc < l then unix_write fd s (i + bc) (l - bc) else ()
      in
      let rec loop buf src dst = match unix_read src buf with
      | 0 -> ()
      | l -> unix_write dst buf 0 l; loop buf src dst
      in
      let buf = match buf with
      | Some b -> b
      | None -> Bytes.create unix_buffer_size
      in
      loop buf src dst

    let to_string fd =
      let b = Bytes.create unix_buffer_size in
      let acc = Buffer.create unix_buffer_size in
      let rec loop () = match Unix.read fd b 0 (Bytes.length b) with
      | 0 -> Buffer.contents acc
      | l -> Buffer.add_subbytes acc b 0 l; loop ()
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
      in
      loop ()

    let rec really_read fd b start len = match len <= 0 with
    | true -> ()
    | false ->
        match Unix.read fd b start len with
        | 0 -> failwith (err_doing "Reading" "Unexpected end of file")
        | r -> really_read fd b (start + r) (len - r)
        | exception Unix.Unix_error (Unix.EINTR, _, _) ->
            really_read fd b start len

    let read_file file fd =
      try
        match Unix.lseek fd 0 Unix.SEEK_END with
        | exception Unix.Unix_error (Unix.ESPIPE, _, _) -> to_string fd
        | len when len > Sys.max_string_length ->
            Fmt.failwith_notrace
              "File to read too large: %d bytes, max supported: %d"
              len Sys.max_string_length
        | len ->
            let b = Bytes.create len in
            ignore (Unix.lseek fd 0 Unix.SEEK_SET);
            really_read fd b 0 len;
            Bytes.unsafe_to_string b
      with
      | Failure e -> Fmt.failwith_notrace "%a: %s" Fpath.pp file e
      | Unix.Unix_error (e, _, _) ->
          Fmt.failwith_notrace "%s: %s" file (err_doing "Reading" (uerr e))

    module Set = struct (* Maintains a set of fds to close. *)
      module Fd = struct
        type t = Unix.file_descr
        let compare : t -> t -> int = compare
      end
      module S = Set.Make (Fd)
      type t = S.t ref
      let empty () = ref S.empty
      let rem fd s = s := S.remove fd !s
      let add fd s = s := S.add fd !s
      let close_all s = S.iter close_no_unix_exn !s; s := S.empty
      let close fd s =
        if S.mem fd !s then (close_no_unix_exn fd; s := S.remove fd !s)
    end
  end

  module Fs_base = struct
    let rec is_dir p = try (Unix.stat p).Unix.st_kind = Unix.S_DIR with
    | Unix.Unix_error (Unix.EINTR, _, _) -> is_dir p

    let rec is_symlink p = try (Unix.lstat p).Unix.st_kind = Unix.S_LNK with
    | Unix.Unix_error (Unix.EINTR, _, _) -> is_symlink p

    let rec unlink p = try Unix.unlink p with
    | Unix.Unix_error (Unix.EINTR,_, _) -> unlink p

    let rec file_delete p = try Ok (Unix.unlink (Fpath.to_string p); true) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
    | Unix.Unix_error (Unix.EINTR, _, _) -> file_delete p
    | Unix.Unix_error (e, _, _) -> ferr p (err_doing "Deleting" (uerr e))

    (* Directory operations. *)

    let dir_create ?(mode = 0o755) ~make_path dir =
      let create_op = "Creating" in
      let mkdir dir mode = Unix.mkdir (Fpath.to_string dir) mode in
      try
        let pmode = 0o755 in
        try Ok (mkdir dir mode; true) with
        | Unix.Unix_error (Unix.EEXIST, _, _) ->
            if is_dir dir then Ok false else
            ferr dir (err_doing create_op "Path exists but not a directory")
        | Unix.Unix_error (Unix.ENOENT, _, _) when make_path ->
            let rec down = function
            | [] -> assert false
            | [dir] ->
                (try Ok (mkdir dir mode; true) with
                | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok false)
            | dir :: dirs ->
                match mkdir dir pmode with
                | () -> down dirs
                | exception Unix.Unix_error (Unix.EEXIST, _, _) -> down dirs
            in
            let rec up todo p = match Unix.mkdir p pmode with
            | () -> down todo
            | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
                up (p :: todo) (Fpath.parent p)
            in
            up [dir] (Fpath.parent dir)
      with
      | Unix.Unix_error (e, _, p) ->
          match String.equal (Fpath.to_string dir) p with
          | true -> ferr dir (err_doing create_op (uerr e))
          | false ->
              let perr = Fmt.str "%s: %s" p (uerr e) in
              ferr dir (err_doing create_op perr)

    let dir_delete ~recurse dir =
      let delete_op = "Deleting" in
      let err e = Fmt.failwith_notrace "%a: %s" Fpath.pp dir e in
      let rec delete_symlink p =
        if is_symlink p then (unlink p; true) else false
      in
      let try_unlink file = match Unix.unlink (Fpath.to_string file) with
      | () -> true
      | exception Unix.Unix_error (e, _, _) ->
          match e with
          | Unix.ENOENT -> true
          | Unix.EISDIR (* Linux *) | Unix.EPERM (* POSIX *) -> false
          | Unix.EACCES when Sys.win32 ->
              (* This is what Unix.unlink returns on directories on Windows. *)
              false
          | e ->
              let ferr = Fmt.str "%a: %s" Fpath.pp file (uerr e) in
              err (err_doing delete_op ferr)
      in
      let rec delete_contents d dh todo = match Unix.readdir dh with
      | exception End_of_file -> d :: todo
      | ".." | "." -> delete_contents d dh todo
      | file ->
          let file = Fpath.(d / file) in
          if try_unlink file then delete_contents d dh todo else
          file :: d :: todo (* file is a dir we'll come back later for [d] *)
      in
      let rec try_delete d todo = match Unix.opendir (Fpath.to_string d) with
      | dh ->
          let dirs = match delete_contents d dh todo with
          | dirs -> Unix.closedir dh; dirs
          | exception e -> Unix.closedir dh; raise e
          in
          doit dirs
      | exception Unix.Unix_error (e, _, _) ->
          match e with
          | Unix.ENOENT | Unix.ENOTDIR -> doit todo
          | e ->
              let derr = Fmt.str "%a: %s" Fpath.pp d (uerr e) in
              err (err_doing delete_op derr)
      and doit = function
      | [] -> ()
      | d :: ds ->
          match Unix.rmdir (Fpath.to_string d) with
          | () -> doit ds
          | exception Unix.Unix_error (e, _, _) ->
              match e with
              | Unix.ENOTEMPTY -> try_delete d ds
              | Unix.ENOENT | Unix.ENOTDIR -> doit ds
              | e ->
                  let derr = Fmt.str "%a: %s" Fpath.pp d (uerr e) in
                  err (err_doing delete_op derr)
      in
      try match Unix.rmdir (Fpath.to_string dir) with
      | () -> Ok true
      | exception Unix.Unix_error (e, _, _) ->
          match e with
          | Unix.ENOTEMPTY when recurse -> Ok (try_delete dir []; true)
          | Unix.ENOENT -> Ok false
          | Unix.ENOTDIR ->
              begin try
                if delete_symlink (Fpath.to_string dir) then Ok true else
                err (err_doing delete_op (uerr Unix.ENOTDIR))
              with
              | Unix.Unix_error (e, _, _) -> err (err_doing delete_op (uerr e))
              end
          | e -> err (err_doing delete_op (uerr e))
      with
      | Failure e -> Result.error e

    (* Handling forced file operations *)

    let err_force p = ferr p "Path exists"

    let rec handle_force ~force file =
      if force then Ok () else
      try ignore (Unix.lstat (Fpath.to_string file)); err_force file with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
      | Unix.Unix_error (Unix.EINTR, _, _) -> handle_force ~force file
      | Unix.Unix_error (e, _, _) ->
          ferr file (err_doing "Testing existence" (uerr e))

    let rec handle_force_open_fdout
      ?(flags = Unix.[O_WRONLY; O_CREAT; O_SHARE_DELETE; O_CLOEXEC; O_TRUNC])
      ~force ~make_path ~mode file
      =
      let fls = if force then flags else Unix.O_EXCL :: flags in
      match Unix.openfile file fls mode with
      | fd -> Ok fd
      | exception Unix.Unix_error (Unix.EEXIST, _, _) -> err_force file
      | exception Unix.Unix_error (Unix.EINTR, _, _) ->
          handle_force_open_fdout ~flags ~force ~make_path ~mode file
      | exception Unix.Unix_error (Unix.ENOENT as e, _, _) when make_path ->
          begin match dir_create ~make_path (Fpath.parent file) with
          | Error e -> ferr file e
          | Ok false (* existed *) -> ferr file (uerr e)
          | Ok true (* created *) ->
              handle_force_open_fdout ~flags ~force ~make_path ~mode file
          end
      | exception Unix.Unix_error (e, _, _) -> ferr file (uerr e)

    (* Path operations *)

    let rec path_exists p =
      try (ignore (Unix.stat (Fpath.to_string p)); Ok true) with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
      | Unix.Unix_error (Unix.EINTR, _, _) -> path_exists p
      | Unix.Unix_error (e, _, _) ->
          ferr p (err_doing "Testing existence" (uerr e))

    let rec path_get_mode p =
      try Ok ((Unix.stat @@ Fpath.to_string p).Unix.st_perm) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> path_get_mode p
      | Unix.Unix_error (e, _, _) ->
          ferr p (err_doing "Getting file mode" (uerr e))

    let rec path_set_mode p m = try Ok (Unix.chmod (Fpath.to_string p) m) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> path_set_mode p m
    | Unix.Unix_error (e, _, _) ->
        ferr p (err_doing "Setting file mode" (uerr e))

    let rec path_delete ~recurse p =
      try Ok (Unix.unlink (Fpath.to_string p); true) with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
      | Unix.Unix_error (Unix.EINTR, _, _) -> path_delete ~recurse p
      | Unix.Unix_error ((Unix.EPERM | Unix.EISDIR), _, _) ->
          dir_delete ~recurse p
      | Unix.Unix_error (e, _, _) -> ferr p (err_doing "Deleting" (uerr e))

    let rec path_rename ~force ~make_path ~src p =
      let err e = Fmt.error "rename %a to %a: %s" Fpath.pp src Fpath.pp p e in
      match handle_force ~force p with
      | Error e -> err e
      | Ok () ->
          try Ok (Unix.rename (Fpath.to_string src) (Fpath.to_string p)) with
          | Unix.Unix_error (Unix.ENOENT as e, _, _) when make_path ->
              begin match dir_create ~make_path (Fpath.parent p) with
              | Error e -> err e
              | Ok false (* existed *) -> err (uerr e)
              | Ok true (* created *) -> path_rename ~force ~make_path ~src p
              end
          | Unix.Unix_error (Unix.EINTR, _, _) ->
              path_rename ~force ~make_path ~src p
          | Unix.Unix_error (e, _, _) -> err (uerr e)

    let rec path_stat p = try Ok (Unix.stat (Fpath.to_string p)) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> path_stat p
    | Unix.Unix_error (e, _, _) -> ferr p (err_doing "stat" (uerr e))

    (* Links *)

    let rec symlink ~force ~make_path ~src p =
      let err e = Fmt.error "symlink %a to %a: %s" Fpath.pp src Fpath.pp p e in
      try Ok (Unix.symlink (Fpath.to_string src) (Fpath.to_string p)) with
      | Unix.Unix_error (Unix.EEXIST, _, _) when force ->
          begin match file_delete p with
          | Error e -> err e
          | Ok _ -> symlink ~force ~make_path ~src p
          end
      | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR as e), _, _)
        when make_path ->
          begin match dir_create ~make_path (Fpath.parent p) with
          | Error e -> ferr p (err_doing "Creating path" e)
          | Ok false (* existed *) -> err (uerr e)
          | Ok true (* created *) -> symlink ~force ~make_path ~src p
          end
      | Unix.Unix_error (Unix.EINTR, _, _) -> symlink ~force ~make_path ~src p
      | Unix.Unix_error (e, _, _) -> err (uerr e)

    let rec symlink_link p =
      try
        let l = Unix.readlink (Fpath.to_string p) in
        match Fpath.of_string l with
        | Ok _ as v -> v
        | Error e -> ferr p (err_doing "Reading symlink" e)
      with
      | Unix.Unix_error (Unix.EINVAL, _, _) -> ferr p "Not a symbolic link"
      | Unix.Unix_error (Unix.EINTR, _, _) -> symlink_link p
      | Unix.Unix_error (e, _, _) -> ferr p (uerr e)

    let rec symlink_stat p = try Ok (Unix.lstat (Fpath.to_string p)) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> symlink_stat p
    | Unix.Unix_error (e, _, _) -> ferr p (err_doing "lstat" (uerr e))

    let copy_symlink ~force ~make_path ~src dst =
      Result.bind (symlink_link src) @@ fun src ->
      symlink ~force ~make_path ~src dst

    let rec try_hardlink src dst =
      try Unix.link (Fpath.to_string src) (Fpath.to_string dst); true with
      | Unix.Unix_error (Unix.EINTR, _, _) -> try_hardlink src dst
      | Unix.Unix_error (Unix.EXDEV, _, _)
      | Unix.Unix_error (Unix.EOPNOTSUPP, _, _)
      | Unix.Unix_error (Unix.ENOSYS, _, _)
      | Unix.Unix_error (Unix.EMLINK, _, _) -> false
  end

  module Tmp = struct

    let delete_file file = ignore (Fs_base.file_delete file)
    let files = ref Fpath.Set.empty
    let add_file file = files := Fpath.Set.add file !files
    let rem_file file = delete_file file; files := Fpath.Set.remove file !files

    let delete_dir dir = ignore (Fs_base.dir_delete ~recurse:true dir)
    let dirs = ref Fpath.Set.empty
    let add_dir dir = dirs := Fpath.Set.add dir !dirs
    let rem_dir dir = delete_dir dir; dirs := Fpath.Set.remove dir !dirs

    let cleanup () =
      Fpath.Set.iter delete_file !files;
      Fpath.Set.iter delete_dir !dirs

    let () = at_exit cleanup

    let default_dir =
      let tmp_from_env var ~default =
        Option.value ~default (Env.find ~empty_to_none:true var)
      in
      let dir = match Sys.win32 with
      | true -> tmp_from_env "TEMP" ~default:Fpath.(v "./")
      | false -> tmp_from_env "TMPDIR" ~default:(Fpath.v "/tmp/")
      in
      ref (Fpath.to_dir_path dir)

    type name = (string -> string, unit, string) format
    let default_name = format_of_string "tmp-%s"

    let rand_num () = Random.State.bits (Lazy.force rand_gen) land 0xFFFFFF
    let rand_str () = Printf.sprintf "%06x" (rand_num ())
    let tmp_path dir name rand =
      match dir.[String.length dir - 1] = Fpath.dir_sep_char with
      | true -> Printf.sprintf ("%s" ^^ name) dir rand
      | false -> Printf.sprintf ("%s%c" ^^ name) dir Fpath.dir_sep_char rand

    let err dir name rand e =
      Fmt.error "tmp file %s: %s" (tmp_path dir name rand) e

    let err_too_many dir name =
      err dir name "XXXXXX" "Too many attempts to create"

    let attempts = 10000
    let open'
        ?(flags = Unix.[O_WRONLY; O_CREAT; O_EXCL; O_SHARE_DELETE; O_CLOEXEC])
        ?(mode = 0o600) ?(make_path = true) ?dir ?(name = default_name) ()
      =
      let dir = match dir with None -> !default_dir | Some d -> d in
      let dir_str = Fpath.to_string (Fpath.to_dir_path dir) in
      let rec loop n = match n with
      | 0 -> err_too_many dir name
      | n ->
          let rand = rand_str () in
          try
            let file = tmp_path dir_str name rand in
            let fd = Unix.openfile file flags mode in
            let file = Fpath.v file in
            (add_file file; Ok (file, fd))
          with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (n - 1)
          | Unix.Unix_error (Unix.EINTR, _, _) -> loop n
          | Unix.Unix_error (Unix.ENOENT as e, _, _) when make_path ->
              begin match Fs_base.dir_create ~make_path dir with
              | Error e -> err dir name rand e
              | Ok true (* created *) -> loop n
              | Ok false (* existed *) -> err dir name rand (uerr e)
              end
          | Unix.Unix_error (e, _, _) -> err dir name rand (uerr e)
      in
      loop attempts

    let mkdir
        ?(mode = 0o700) ?(make_path = true) ?dir ?(name = default_name) ()
      =
      let dir = match dir with None -> !default_dir | Some d -> d in
      let dir_str = Fpath.to_string dir in
      let rec loop n = match n with
      | 0 -> err_too_many dir name
      | n ->
          let rand = rand_str () in
          try
            let tdir = tmp_path dir_str name rand in
            let () = Unix.mkdir tdir mode in
            let tdir = Fpath.v tdir in
            (add_dir tdir; Ok tdir)
          with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (n - 1)
          | Unix.Unix_error (Unix.EINTR, _, _) -> loop n
          | Unix.Unix_error (Unix.ENOENT as e, _, _) when make_path ->
              begin match Fs_base.dir_create ~make_path dir with
              | Error e -> err dir name rand e
              | Ok true (* created *) -> loop n
              | Ok false (* existed *) -> err dir name rand (uerr e)
              end
          | Unix.Unix_error (e, _, _) -> err dir name rand (uerr e)
      in
      loop attempts

    let path ?(make_path = true) ?dir ?(name = format_of_string "tmp-%s") () =
      let dir = match dir with None -> !default_dir | Some d -> d in
      let dir_str = Fpath.to_string dir in
      let rec loop n = match n with
      | 0 -> err_too_many dir name
      | n ->
          let rand = rand_str () in
          let file = tmp_path dir_str name rand in
          match Unix.access file [Unix.F_OK] with
          | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok file
          | exception Unix.Unix_error (e, _, _) -> err dir name rand (uerr e)
          | _ -> loop (n - 1)
      in
      if not make_path then loop attempts else
      match Fs_base.dir_create ~make_path dir with
      | Error _ as e -> e
      | Ok _ -> loop attempts
  end

  module File = struct
    let channel_apply ~close c f =
      let close c = try close c with Sys_error _ -> () in
      match f c with v -> close c; v | exception e -> close c; raise e

    (* Famous file paths *)

    let null = Fpath.v (if Sys.win32 then "NUL" else "/dev/null")
    let dash = Fpath.v "-"
    let is_dash = Fpath.equal dash

    (* Existence *)

    let rec exists file =
      match (Unix.stat (Fpath.to_string file)).Unix.st_kind with
      | Unix.S_REG -> Ok true
      | _ -> Ok false
      | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> exists file
      | exception Unix.Unix_error (e, _, _) ->
          ferr file (err_doing "Testing existence" (uerr e))

    let rec must_exist file =
      match (Unix.stat (Fpath.to_string file)).Unix.st_kind with
      | Unix.S_REG -> Ok ()
      | _ ->
          ferr file "File must exist but not a regular file"
      | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
          ferr file "File must exist but no such file"
      | exception Unix.Unix_error (Unix.EINTR, _, _) ->
          must_exist file
      | exception Unix.Unix_error (e, _, _) ->
          ferr file (err_doing "Testing existence" (uerr e))

    let is_executable file = match Unix.access file [Unix.X_OK] with
    | () -> true
    | exception Unix.Unix_error _ -> false

    (* Deleting and truncating *)

    let delete = Fs_base.file_delete
    let rec truncate file size =
      try Ok (Unix.truncate (Fpath.to_string file) size) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> truncate file size
      | Unix.Unix_error (e, _, _) ->
          ferr file (err_doing "Truncating" (uerr e))

    (* Hard links *)

    let rec link ~force ~make_path ~src file =
      let err e = Fmt.error "link %a to %a: %s" Fpath.pp src Fpath.pp file e in
      try Ok (Unix.link (Fpath.to_string src) (Fpath.to_string file)) with
      | Unix.Unix_error (Unix.EEXIST, _, _) when force ->
          begin match delete file with
          | Error e -> err e
          | Ok _ -> link ~force ~make_path ~src file
          end
      | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR as e), _, _)
        when make_path ->
          begin match Fs_base.dir_create ~make_path (Fpath.parent file) with
          | Error e -> ferr file (err_doing "Creating path" e)
          | Ok false (* existed *) -> err (uerr e)
          | Ok true (* created *) -> link ~force ~make_path ~src file
          end
      | Unix.Unix_error (Unix.EINTR, _, _) -> link ~force ~make_path ~src file
      | Unix.Unix_error (e, _, _) -> err (uerr e)

    (* Reads *)

    let read_with_ic file f =
      try
        let ic, close = match is_dash file with
        | true -> stdin, fun _ -> ()
        | false -> open_in_bin (Fpath.to_string file), close_in
        in
        Ok (channel_apply ~close ic f)
      with
      | Sys_error e -> Result.error e

    let read_with_fd file f =
      try
        let fdin, close = match is_dash file with
        | true -> Unix.stdin, (fun _ -> ())
        | false ->
            Fd.openfile (Fpath.to_string file) Unix.[O_RDONLY] 0, Unix.close
        in
        Ok (Fd.apply ~close fdin f)
      with
      | Unix.Unix_error (e, _, _) -> ferr file (uerr e)

    let read_stdin () =
      let b = Bytes.create Fd.unix_buffer_size in
      let acc = Buffer.create Fd.unix_buffer_size in
      let rec loop () = match input stdin b 0 (Bytes.length b) with
      | 0 -> Buffer.contents acc
      | n -> Buffer.add_subbytes acc b 0 n; loop ()
      in
      loop ()

    let read_file file ic = match in_channel_length ic with
    | len when len > Sys.max_string_length ->
        Fmt.failwith_notrace
          "File to read too large: %d bytes, max supported: %d"
          len Sys.max_string_length
    | len ->
        let s = Bytes.create len in
        really_input ic s 0 len;
        Bytes.unsafe_to_string s

    let read file =
      let input c = if c == stdin then read_stdin () else read_file file c in
      try read_with_ic file input with
      | Failure e | Sys_error e -> ferr file e

    (* Writes *)

    let with_tmp_fd ?flags ?mode ?make_path ?dir ?name f =
      Result.bind (Tmp.open' ?flags ?mode ?make_path ?dir ?name ()) @@
      fun (file, fd) ->
      let delete_close fd = Tmp.rem_file file; Unix.close fd in
      Ok (Fd.apply ~close:delete_close fd (f file))

    let open_tmp_fd = Tmp.open'

    let with_tmp_oc ?flags ?mode ?make_path ?dir ?name f =
      Result.bind (Tmp.open' ?flags ?mode ?make_path ?dir ?name ()) @@
      fun (file, fd) ->
      let oc = Unix.out_channel_of_descr fd in
      let delete_close oc = Tmp.rem_file file; close_out oc in
      Ok (channel_apply ~close:delete_close oc (f file))

    let rec rename_tmp src dst =
      try Ok (Unix.rename (Fpath.to_string src) (Fpath.to_string dst)) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> rename_tmp src dst
      | Unix.Unix_error (e, _, _) ->
          let r = Fmt.str "renaming %a to %a" Fpath.pp src Fpath.pp dst in
          Result.error (err_doing r (uerr e))

    let write_op = "Writing"

    let write_with_fd_atomic ~mode ~force ~make_path ~file f =
      Result.bind (Fs_base.handle_force ~force file) @@ fun () ->
      let do_write tmp tmp_oc = match f tmp_oc with
      | Error _ as v -> Ok v
      | Ok _ as v -> Result.map (fun () -> v) (rename_tmp tmp file)
      in
      match with_tmp_fd ~mode ~make_path ~dir:(Fpath.parent file) do_write with
      | Ok v -> v
      | Error e -> ferr file (err_doing write_op e)

    let write_with_fd
        ?(atomic = true) ?(mode = 0o644) ~force ~make_path file f
      =
      match is_dash file with
      | true -> Ok (Fd.apply ~close:(fun _ -> ()) Unix.stdout f)
      | false when atomic ->
          write_with_fd_atomic ~mode ~force ~make_path ~file f
      | false ->
          Result.bind
            (Fs_base.handle_force_open_fdout ~force ~make_path ~mode file) @@
          fun fd -> Ok (Fd.apply ~close:Unix.close fd f)

    let write_with_oc_atomic ~mode ~force ~make_path ~file f =
      Result.bind (Fs_base.handle_force force file) @@ fun () ->
      let do_write tmp tmp_oc = match f tmp_oc with
      | Error _ as v -> Ok v
      | Ok _ as v -> Result.map (fun () -> v) (rename_tmp tmp file)
      in
      match with_tmp_oc ~mode ~make_path ~dir:(Fpath.parent file) do_write with
      | Ok v -> v
      | Error e -> ferr file (err_doing write_op e)

    let write_with_oc
        ?(atomic = true) ?(mode = 0o644) ~force ~make_path file f
      =
      match is_dash file with
      | true -> Ok (channel_apply ~close:(fun _ -> ()) stdout f)
      | false when atomic ->
          write_with_oc_atomic ~mode ~force ~make_path ~file f
      | false ->
          Result.bind
            (Fs_base.handle_force_open_fdout ~force ~make_path ~mode file) @@
          fun fd ->
          let oc = Unix.out_channel_of_descr fd in
          Ok (channel_apply ~close:close_out oc f)

    let write ?atomic ?mode ~force ~make_path file data =
      let out data oc = Ok (output_string oc data) in
      try
        Result.join @@
        write_with_oc ?atomic ?mode ~force ~make_path file (out data)
      with
      | Sys_error e -> ferr file e

    let copy ?atomic ?mode ~force ~make_path ~src file =
      let err e = Fmt.str "copy %a to %a: %s" Fpath.pp src Fpath.pp file e in
      Result.map_error err @@ Result.join @@
      read_with_fd src @@ fun fdi ->
      try match is_dash file with
      | true -> Ok (Fd.copy fdi Unix.stdout)
      | false ->
          let mode = match mode with
          | None -> Fs_base.path_get_mode src
          | Some m -> Ok m
          in
          Result.join @@ Result.bind mode @@ fun mode ->
          write_with_fd ?atomic ~mode ~force ~make_path file @@ fun fdo ->
          Ok (Fd.copy fdi fdo)
      with
      | Unix.Unix_error (e, _, arg) -> Fmt.error "%s: %s" arg (uerr e)
  end

  module Dir = struct

    (* Existence *)

    let rec exists dir =
      match (Unix.stat @@ Fpath.to_string dir).Unix.st_kind with
      | Unix.S_DIR -> Ok true
      | _ -> Ok false
      | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> exists dir
      | exception Unix.Unix_error (e, _, _) ->
          ferr dir (err_doing "Testing existence" (uerr e))

    let rec must_exist dir =
      match (Unix.stat @@ Fpath.to_string dir).Unix.st_kind with
      | Unix.S_DIR -> Ok ()
      | _ ->
          ferr dir "Directory must exist but not a directory"
      | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
          ferr dir "Directory must exist but no such directory"
      | exception Unix.Unix_error (Unix.EINTR, _, _) ->
          must_exist dir
      | exception Unix.Unix_error (e, _, _) ->
          ferr dir (err_doing "Testing existence" (uerr e))

    (* Creating, deleting and renaming. *)

    let create = Fs_base.dir_create

    (* Contents *)

    let rec readdir ~dotfiles dir =
      let is_dot_file s = String.length s <> 0 && s.[0] = '.' in
      let rec loop ~dotfiles dir dh acc = match Unix.readdir dh with
      | exception End_of_file -> acc
      | ".." | "." -> loop ~dotfiles dir dh acc
      | n when is_dot_file n && not dotfiles -> loop ~dotfiles dir dh acc
      | n when Fpath.is_seg n -> loop ~dotfiles dir dh (n :: acc)
      | n -> ffail dir (Fmt.str "%S: Invalid file name" n)
      in
      let dh = Unix.opendir (Fpath.to_string dir) in
      match loop ~dotfiles dir dh [] with
      | fs -> Unix.closedir dh; fs
      | exception e ->
          (try Unix.closedir dh with Unix.Unix_error (_, _, _) -> ());
          raise e

    let rec stat p = try (Unix.stat @@ Fpath.to_string p) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> stat p

    let rec lstat p = try (Unix.lstat @@ Fpath.to_string p) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> lstat p

    let fold_no_rec ~filter ~rel ~dotfiles ~follow_symlinks dir f acc =
      let rec loop stat f acc adir = function
      | [] -> Ok acc
      | n :: ns ->
          let full = Fpath.(adir / n) in
          match stat full with
          | st ->
              begin match st.Unix.st_kind with
              | Unix.S_DIR ->
                  if filter = `Non_dir then loop stat f acc adir ns else
                  let p = if rel then Fpath.v n else full in
                  loop stat f (f st n p acc) adir ns
              | _ when filter <> `Dir ->
                  let p = if rel then Fpath.v n else full in
                  loop stat f (f st n p acc) adir ns
              | _ ->
                  loop stat f acc adir ns
              end
          | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
              loop stat f acc adir ns
          | exception Unix.Unix_error (Unix.ENOTDIR, _, _) ->
              loop stat f acc adir ns
      in
      let stat = if follow_symlinks then stat else lstat in
      loop stat f acc dir (readdir ~dotfiles dir)

    let fold_rec ~filter ~rel ~dotfiles ~follow_symlinks ~prune dir f acc =
      let rec loop stat todo adir rdir f acc = function
      | [] ->
          begin match todo with
          | (dir, rdir, ns) :: todo -> loop stat todo dir rdir f acc ns
          | [] -> Ok acc
          end
      | n :: ns ->
          let full = Fpath.(adir / n) in
          begin match stat full with
          | st ->
              begin match st.Unix.st_kind with
              | Unix.S_DIR ->
                  let rp = match rdir with
                  | None -> Fpath.v n | Some rdir -> Fpath.(rdir / n)
                  in
                  let p = if not rel then full else rp in
                  if prune st n p
                  then loop stat todo adir rdir f acc ns else
                  let acc = if filter = `Non_dir then acc else f st n p acc in
                  let todo = (adir, rdir, ns) :: todo in
                  loop stat todo full (Some rp) f acc (readdir ~dotfiles full)
              | _ when filter <> `Dir ->
                  let p = if not rel then full else match rdir with
                  | None -> Fpath.v n | Some rdir -> Fpath.(rdir / n)
                  in
                  loop stat todo adir rdir f (f st n p acc) ns
              | _ ->
                  loop stat todo adir rdir f acc ns
              end
          | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
              loop stat todo adir rdir f acc ns
          | exception Unix.Unix_error (Unix.ENOTDIR, _, _) ->
              loop stat todo adir rdir f acc ns
          end
      in
      let stat = if follow_symlinks then stat else lstat in
      loop stat [] dir None f acc (readdir ~dotfiles dir)

    let _fold
        ~(filter : [`Any | `Non_dir | `Dir]) ?(rel = false) ?(dotfiles = false)
        ?(follow_symlinks = true) ?(prune = fun _ _ _ -> false) ~recurse
        f dir acc
      =
      let listing_op = "Listing" in
      try
        if recurse
        then fold_rec ~filter ~rel ~dotfiles ~follow_symlinks ~prune dir f acc
        else fold_no_rec ~filter ~rel ~dotfiles ~follow_symlinks dir f acc
      with
      | Failure e -> ferr dir (err_doing listing_op e)
      | Unix.Unix_error (e, _, ep) ->
          if String.equal (Fpath.to_string dir) ep
          then ferr dir (err_doing listing_op @@ uerr e)
          else ferr dir (err_doing listing_op @@ Fmt.str "%s: %s" ep (uerr e))

    let fold ?rel ?dotfiles ?follow_symlinks ?prune ~recurse f dir acc =
      _fold ~filter:`Any ?rel ?dotfiles ?follow_symlinks ?prune ~recurse
        f dir acc

    let fold_files ?rel ?dotfiles ?follow_symlinks ?prune ~recurse f dir acc =
      _fold ~filter:`Non_dir ?rel ?dotfiles ?follow_symlinks ?prune ~recurse
        f dir acc

    let fold_dirs ?rel ?dotfiles ?follow_symlinks ?prune ~recurse f dir acc =
      _fold ~filter:`Dir ?rel ?dotfiles ?follow_symlinks ?prune ~recurse
        f dir acc

    let path_list stat _ f acc = match stat.Unix.st_kind with
    | Unix.S_DIR -> Fpath.to_dir_path f :: acc
    | _ -> f :: acc

    (* copy *)

    let copy
        ?(rel = true) ?(atomic = true) ?(allow_hardlinks = true)
        ?(follow_symlinks = true) ?(prune = fun _ _ _ -> false)
        ~make_path ~recurse ~src dst
      =
      let err e = Fmt.str "copy %a to %a: %s" Fpath.pp src Fpath.pp dst e in
      let prune = match rel with (* we invoke [_fold] with [rel:true] *)
      | true -> prune
      | false -> fun st name p -> prune st name (Fpath.(src // p))
      in
      let copy dst st name p (hard, chmods as acc) = match st.Unix.st_kind with
      | Unix.S_DIR (* prune was already called on it *) ->
          let dst = Fpath.(dst // p) in
          let mode = st.Unix.st_perm in
          let writeable = (mode land 0o200 <> 0) in
          let mode, acc = match writeable with
          | true -> mode, acc
          | false ->
              (* We need to be able to write to the directory, we remember
                 the mode and the dir and set it at the end *)
              0o700, (hard, (dst, mode) :: chmods)
          in
          ignore (Fs_base.dir_create ~mode ~make_path:false dst |>
                  Result.to_failure);
          acc
      | Unix.S_REG ->
          let cp ~mode src dst =
            Result.join @@
            File.read_with_fd src @@ fun fdi ->
            Result.join @@
            File.write_with_fd
              ~atomic:true ~mode ~force:false ~make_path:false dst @@
            fun fdo -> Ok (Fd.copy fdi fdo)
          in
          if prune st name p then acc else
          let mode = st.Unix.st_perm in
          let src = Fpath.(src // p) in
          let dst = Fpath.(dst // p) in
          if not hard then ((cp ~mode src dst |> Result.to_failure); acc) else
          if Fs_base.try_hardlink src dst then acc else
          (cp ~mode src dst |> Result.to_failure; (false, chmods))
      | Unix.S_LNK ->
          if prune st name p then acc else
          let dst = Fpath.(dst // p) in
          let src = Fpath.(src // p) in
          let force = false and make_path = false in
          Fs_base.copy_symlink ~force ~make_path ~src dst |> Result.to_failure;
          acc
      | _ when prune st name p (* why not *) -> acc
      | _ ->
          Fmt.failwith "%a: Not a regular file, directory or symlink"
            Fpath.pp Fpath.(src // p)
      in
      let rec chmod_dirs = function
      | [] -> ()
      | (d, m) :: ds ->
          (Fs_base.path_set_mode d m) |> Result.to_failure; chmod_dirs ds
      in
      Result.map_error err @@
      Result.bind (Fs_base.path_exists dst) @@ function
      | true -> Error "Destination path already exists"
      | false ->
          let tdst = match atomic with
          | true -> Tmp.mkdir ~make_path ~dir:(Fpath.parent dst) ()
          | false ->
              Result.bind (Fs_base.dir_create ~make_path dst) @@
              fun _ -> Ok dst
          in
          Result.bind tdst @@ fun tdst ->
          try
            let src_mode = Fs_base.path_get_mode src |> Result.to_failure in
            let _, chmods =
              _fold ~filter:`Any ~rel:true ~dotfiles:true ~follow_symlinks
                ~prune ~recurse (copy tdst) src
                (allow_hardlinks, [src, src_mode])
              |> Result.to_failure
            in
            chmod_dirs chmods;
            match atomic with
            | false -> Ok ()
            | true ->
                Fs_base.path_rename ~force:false ~make_path:true ~src:tdst dst
          with Failure e ->
            if atomic then ignore (Fs_base.path_delete ~recurse:true tdst);
            Error e

    (* Default temporary directory *)

    let set_default_tmp p = Tmp.default_dir := Fpath.to_dir_path p
    let default_tmp () = !Tmp.default_dir

    (* Temporary directories *)

    let with_tmp ?mode ?make_path ?dir ?name f =
      Result.bind (Tmp.mkdir ?mode ?make_path ?dir ?name ()) @@
      fun dir ->
      try let v = f dir in Tmp.rem_dir dir; Ok v with
      | e -> Tmp.rem_dir dir; raise e

    let tmp = Tmp.mkdir

    (* Current working directory *)

    let rec cwd () =
      let err e = Fmt.error "get cwd: %s" e in
      match Fpath.of_string (Unix.getcwd ()) with
      | Ok dir when Fpath.is_abs dir -> Ok dir
      | Ok dir -> err (Fmt.str "%a is relative" Fpath.pp dir)
      | Error e -> err e
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> cwd ()
      | exception Unix.Unix_error (e, _, _) -> err (uerr e)

    let rec set_cwd dir =
      let err e = Fmt.error "set cwd to %a: %s" Fpath.pp dir e in
      try Ok (Unix.chdir (Fpath.to_string dir)) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> set_cwd dir
      | Unix.Unix_error (e, _, _) -> err (uerr e)

    let with_cwd dir f =
      Result.bind (cwd ()) @@ fun old ->
      Result.bind (set_cwd dir) @@ fun () ->
      match f () with
      | v -> Result.map (fun () -> v) (set_cwd old)
      | exception e -> ignore (set_cwd old); raise e

    (* Base directories *)

    let err_dir dir fmt = Fmt.error ("%s directory: " ^^ fmt) dir
    let fpath_of_env_var dir var = match Env.find ~empty_to_none:true var with
    | None -> None
    | Some p ->
        match Fpath.of_string p with
        | Error e -> Some (err_dir dir "%s environment variable: %s" var e)
        | Ok _ as v -> Some v

    let base_dir dir var var_alt fallback = match fpath_of_env_var dir var with
    | Some r -> r
    | None ->
        match Option.bind var_alt (fpath_of_env_var dir) with
        | Some r -> r
        | None -> fallback ()

    let home_dir = "user"
    let home_var = "HOME"
    let user () =
      let home_env home_var = match fpath_of_env_var home_dir home_var with
      | Some r -> r
      | None -> err_dir home_dir "%s environment variable is undefined" home_var
      in
(*      if Sys.win32 then home_env home_win32_var else *)
      match Fpath.of_string (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir with
      | Ok _ as v -> v
      | Error _ -> home_env home_var
      | exception Not_found -> home_env home_var
      | exception Unix.Unix_error (e, _, _) -> home_env home_var

    let home_fallback dir sub = match user () with
    | Error e -> err_dir dir "%s" e
    | Ok home -> Ok Fpath.(home // sub)

    let config_dir = "configuration"
    let config_var = "XDG_CONFIG_HOME"
    let config_var_alt = if Sys.win32 then Some "%APPDATA%" else None
    let config_fallback () = home_fallback config_dir (Fpath.v ".config")
    let config () =
      base_dir config_dir config_var config_var_alt config_fallback

    let data_dir = "data"
    let data_var = "XDG_DATA_HOME"
    let data_var_alt = if Sys.win32 then Some "%APPDATA%" else None
    let data_fallback () = home_fallback data_dir (Fpath.v ".local/share")
    let data () =
      base_dir data_dir data_var data_var_alt data_fallback

    let cache_dir = "cache"
    let cache_var = "XDG_CACHE_HOME"
    let cache_var_alt = if Sys.win32 then Some "%TEMP%" else None
    let cache_fallback () = home_fallback cache_dir (Fpath.v ".cache")
    let cache () =
      base_dir cache_dir cache_var cache_var_alt cache_fallback

    let runtime_dir = "runtime"
    let runtime_var = "XDG_RUNTIME_HOME"
    let runtime_var_alt = None
    let runtime_fallback () = Ok (default_tmp ())
    let runtime () =
      base_dir runtime_dir runtime_var runtime_var_alt runtime_fallback
  end

  module Path = struct

    (* Existence *)

    let exists = Fs_base.path_exists

    let rec must_exist p =
      try (Ok (ignore (Unix.stat (Fpath.to_string p)))) with
      | Unix.Unix_error (Unix.ENOENT, _, _) ->
          ferr p "Path must exist but no such path"
      | Unix.Unix_error (Unix.EINTR, _, _) -> must_exist p
      | Unix.Unix_error (e, _, _) ->
          ferr p (err_doing "Testing existence" (uerr e))

    (* Deleting and renaming *)

    let delete = Fs_base.path_delete
    let rename = Fs_base.path_rename

    (* Copying *)

    let copy
        ?(rel = true) ?(atomic = true) ?(allow_hardlinks = true)
        ?(follow_symlinks = true) ?(prune = fun _ _ _ -> false)
        ~make_path ~recurse ~src dst
      =
      let err e = Fmt.str "copy %a to %a: %s" Fpath.pp src Fpath.pp dst e in
      let stat = match follow_symlinks with
      | true -> Fs_base.path_stat
      | false -> Fs_base.symlink_stat
      in
      match stat src with
      | Error e -> Error (err e)
      | Ok stat ->
          match stat.Unix.st_kind with
          | Unix.S_DIR ->
              Dir.copy
                ~rel ~atomic ~allow_hardlinks ~follow_symlinks ~prune
                ~make_path ~recurse ~src dst
          | Unix.S_LNK ->
              Result.map_error err @@
              Fs_base.copy_symlink ~force:false ~make_path ~src dst
          | _  ->
              match allow_hardlinks with
              | false -> File.copy ~atomic ~force:false ~make_path ~src dst
              | true ->
                  match File.link ~force:false ~make_path ~src dst with
                  | Ok _ -> Ok ()
                  | Error _ ->
                      File.copy ~atomic ~force:false ~make_path ~src dst

    (* File modes and stat *)

    let get_mode = Fs_base.path_get_mode
    let set_mode = Fs_base.path_set_mode
    let stat = Fs_base.path_stat

    (* Symlinks *)

    let symlink = Fs_base.symlink
    let symlink_link = Fs_base.symlink_link
    let symlink_stat = Fs_base.symlink_stat

    (* Temporary paths *)

    type tmp_name = Tmp.name
    let tmp = Tmp.path
  end

  module Cmd = struct

    (* Tool search *)

    let tool_file ~dir tool = match dir.[String.length dir - 1] with
    | c when Fpath.char_is_dir_sep c -> dir ^ tool
    | _ -> String.concat Fpath.dir_sep [dir; tool]

    let search_in_path tool =
      let rec loop tool = function
      | "" -> None
      | p ->
          let dir, p = match String.cut_left ~sep:Fpath.search_path_sep p with
          | None -> p, ""
          | Some (dir, p) -> dir, p
          in
          if dir = "" then loop tool p else
          let tool_file = tool_file ~dir tool in
          match File.is_executable tool_file with
          | false -> loop tool p
          | true -> Some (Fpath.v tool_file)
      in
      match Unix.getenv "PATH" with
      | p -> loop tool p
      | exception Not_found -> None

    let search_in_dirs ~dirs tool =
      let rec loop tool = function
      | [] -> None
      | d :: dirs ->
          let tool_file = tool_file ~dir:(Fpath.to_string d) tool in
          match File.is_executable tool_file with
          | false -> loop tool dirs
          | true -> Some (Fpath.v tool_file)
      in
      loop tool dirs

    let ensure_exe_suffix_if_win32 = match Sys.win32 with
    | false -> fun t -> t
    | true ->
        fun t -> match String.is_suffix ~affix:".exe" t with
        | true -> t
        | false -> t ^ ".exe"

    let tool_is_path t =
      try ignore (String.index t Fpath.dir_sep_char); true with
      | Not_found -> false

    let find_tool ?search tool =
      let tool = ensure_exe_suffix_if_win32 tool in
      match tool_is_path tool with
      | true -> if File.is_executable tool then Ok (Some tool) else Ok None
      | false ->
          match search with
          | None -> Ok (search_in_path tool)
          | Some dirs -> Ok (search_in_dirs ~dirs tool)

    let must_find_tool ?search tool = match find_tool ?search tool with
    | Ok (Some t) -> Ok t
    | Error _ as e -> e
    | Ok None when tool_is_path tool ->
        Fmt.error "%s: No such executable file" tool
    | Ok None ->
        let pp_search ppf = function
        | None -> Fmt.string ppf "PATH"
        | Some dirs ->Fmt.(list ~sep:comma Fpath.pp) ppf dirs
        in
        Fmt.error "%s: No such tool found in %a" tool pp_search search

    let rec find_first_tool ?search = function
    | [] -> Ok None
    | tool :: tools ->
        match find_tool ?search tool with
        | Ok None -> find_first_tool ?search tools
        | v -> v

    let find ?search cmd = match Cmd.tool cmd with
    | None -> Ok None
    | Some tool ->
        Result.bind (find_tool ?search tool) @@ function
        | Some tool -> Ok (Cmd.set_tool tool cmd)
        | None -> Ok None

    let must_find ?search cmd = match Cmd.tool cmd with
    | None -> Fmt.error "%a: No tool specified" Cmd.pp_dump cmd
    | Some tool ->
        Result.bind (must_find_tool ?search tool) @@ fun tool ->
        Ok (match Cmd.set_tool tool cmd with None -> assert false | Some c -> c)

    let rec find_first ?search = function
    | [] -> Ok None
    | cmd :: cmds ->
        match find ?search cmd with
        | Ok None -> find_first ?search cmds
        | v -> v

    (* Process completion statuses *)

    type status = [ `Exited of int | `Signaled of int ]

    let status_of_unix_status = function
    | Unix.WEXITED e -> `Exited e
    | Unix.WSIGNALED s -> `Signaled s
    | Unix.WSTOPPED _ -> assert false

    let pp_status ppf = function
    | `Exited n -> Fmt.pf ppf "@[exited [%d]@]" n
    | `Signaled s -> Fmt.pf ppf "@[signaled [%a]@]" Fmt.sys_signal s

    let pp_cmd_status ppf (cmd, st) =
      Fmt.pf ppf "cmd [%s]: %a" (Cmd.to_string cmd) pp_status st

    (* Process standard inputs *)

    type stdi =
    | In_string of string
    | In_file of Fpath.t
    | In_fd of { fd : Unix.file_descr; close : bool }

    let in_string s = In_string s
    let in_file f = In_file f
    let in_fd ~close fd = In_fd { fd; close }
    let in_stdin = In_fd { fd = Unix.stdin; close = false }
    let in_null = In_file File.null
    let stdi_to_fd fds = function
    | In_fd { fd; close } -> if close then Fd.Set.add fd fds; fd
    | In_string s ->
        begin try
          (* We write the input string to a temporary file. *)
          let flags = Unix.[O_RDWR; O_CREAT; O_EXCL; O_SHARE_DELETE] in
          let f, fd = Result.to_failure (Tmp.open' ~flags ()) in
          Fd.Set.add fd fds;
          Tmp.rem_file f; (* We don't need the actual file. *)
          ignore (Unix.write_substring fd s 0 (String.length s));
          ignore (Unix.lseek fd 0 Unix.SEEK_SET);
          fd
        with
        | Unix.Unix_error (e, _, _) ->
            Fmt.failwith_notrace "tmp file for stdin: %s" (uerr e)
        end
    | In_file f ->
        try
          let f = Fpath.to_string f in
          let fd = Fd.openfile f Unix.[O_RDONLY] 0o644 in
          Fd.Set.add fd fds; fd
        with Unix.Unix_error (e, _, _) ->
          Fmt.failwith_notrace "open file %a for stdin: %s" Fpath.pp f (uerr e)

    (* Process standard outputs *)

    type stdo =
    | Out_fd of { fd : Unix.file_descr; close : bool }
    | Out_file of
        { mode : int; force : bool; make_path : bool; file : Fpath.t }

    let out_file ?(mode = 0o644) ~force ~make_path file =
      Out_file { mode; force; make_path; file }

    let out_fd ~close fd = Out_fd { fd; close }
    let out_stdout = Out_fd { fd = Unix.stdout; close = false }
    let out_stderr = Out_fd { fd = Unix.stderr; close = false }
    let out_null = out_file ~force:true ~make_path:false File.null

    let stdo_to_fd fds = function
    | Out_fd { fd; close } -> if close then Fd.Set.add fd fds; fd
    | Out_file { mode; force; make_path; file } ->
        let flags = Unix.[O_WRONLY; O_CREAT; O_TRUNC] in
        match
          Fs_base.handle_force_open_fdout ~flags ~force ~make_path ~mode file
        with
        | Error e -> Fmt.failwith_notrace "open for output: %s" e
        | Ok fd -> Fd.Set.add fd fds; fd

    (* Low-level command spawn *)

    type spawn_tracer =
      int option -> Env.assignments option -> cwd:Fpath.t option -> Cmd.t ->
      unit

    let spawn_tracer_nop _ _ ~cwd:_ _ = ()
    let _spawn_tracer = ref spawn_tracer_nop
    let spawn_tracer () = !_spawn_tracer
    let set_spawn_tracer t = _spawn_tracer := t

    let rec getcwd () = try Unix.getcwd () with
    | Unix.Unix_error (Unix.EINTR, _, _) -> getcwd ()
    | Unix.Unix_error (e, _, _) -> Fmt.failwith_notrace "getcwd: %s" (uerr e)

    let rec chdir cwd = try Unix.chdir cwd with
    | Unix.Unix_error (Unix.EINTR, _, _) -> chdir cwd
    | Unix.Unix_error (e, _, _) ->
        Fmt.failwith_notrace "chdir %s: %s" cwd (uerr e)

    let spawn_err cmd e = match Cmd.is_empty cmd with
    | true -> Result.error e
    | false -> Fmt.error "cmd %s: %s" (Cmd.to_string cmd) e

    let spawn_cwd = function None -> getcwd () | Some d -> Fpath.to_string d
    let spawn_env = function
    | None -> Unix.environment ()
    | Some e -> Array.of_list e

    let _spawn fds ?env ?cwd ~stdin ~stdout ~stderr cmd =
      match Cmd.to_list cmd with
      | [] -> failwith "no command, empty command line"
      | line ->
          try
            let env' = spawn_env env in
            let cwd' = spawn_cwd cwd in
            let line = Array.of_list line in
            let exe = line.(0) in
            let stdin = stdi_to_fd fds stdin in
            let stdout = stdo_to_fd fds stdout in
            let stderr = stdo_to_fd fds stderr in
            let old_cwd = getcwd () in
            let change_cwd = not @@ String.equal old_cwd cwd' in
            if change_cwd then chdir cwd';
            let pid = Unix.create_process_env exe line env' stdin stdout stderr
            in
            if change_cwd then chdir old_cwd; (* XXX pid zombie on fail. *)
            Fd.Set.close_all fds;
            !_spawn_tracer (Some pid) env cwd cmd;
            pid
          with
          | e ->
              (* In case one of the std{i,o}_to_fd raises *)
              let add_out_fd fds = function
              | Out_fd { fd ; close = true } -> Fd.Set.add fd fds
              | _ -> ()
              in
              add_out_fd fds stdout; add_out_fd fds stderr;
              raise e

    (* Blocking command execution *)

    let rec run_collect pid = match Unix.waitpid [] pid with
    | _, status -> status_of_unix_status status
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> run_collect pid
    | exception Unix.Unix_error (e, _, _) ->
        Fmt.failwith_notrace "waitpid [%d]: %s" pid (uerr e)

    let run_status
        ?env ?cwd ?(stdin = in_stdin) ?(stdout = out_stdout)
        ?(stderr = out_stderr) cmd
      =
      let fds = Fd.Set.empty () in
      try
        let pid = _spawn fds ?env ?cwd ~stdin ~stdout ~stderr cmd in
        Ok (run_collect pid)
      with
      | Failure e -> Fd.Set.close_all fds; spawn_err cmd e
      | Unix.Unix_error (e, _, _) ->
          Fd.Set.close_all fds; spawn_err cmd (uerr e)

    let run_status_out
        ?env ?cwd ?(stdin = in_stdin) ?(stderr = `Stdo out_stderr)
        ?(trim = true) cmd
      =
      let fds = Fd.Set.empty () in
      try
        let flags = Unix.[O_RDWR; O_CREAT; O_EXCL; O_SHARE_DELETE; O_CLOEXEC] in
        let tmpf, fd = Result.to_failure (Tmp.open' ~flags ()) in
        let stdout = out_fd ~close:false fd in
        let stderr = match stderr with `Out -> stdout | `Stdo o -> o in
        let pid = _spawn fds ?env ?cwd ~stdin ~stdout ~stderr cmd in
        let status = run_collect pid in
        let out = Fd.read_file tmpf fd in
        let out = if trim then String.trim out else out in
        Tmp.rem_file tmpf;
        Ok (status, out)
      with
      | Failure e -> Fd.Set.close_all fds; spawn_err cmd e
      | Unix.Unix_error (e, _, _) ->
          Fd.Set.close_all fds; spawn_err cmd (uerr e)

    let run ?env ?cwd ?stdin ?stdout ?stderr cmd =
      match run_status ?env ?cwd ?stdin ?stdout ?stderr cmd with
      | Ok (`Exited 0) -> Ok ()
      | Ok st -> Fmt.error "%a" pp_cmd_status (cmd, st)
      | Error _ as e -> e

    let run_out ?env ?cwd ?stdin ?stderr ?trim cmd =
      match run_status_out ?env ?cwd ?stdin ?stderr ?trim cmd with
      | Ok (`Exited 0, v) -> Ok v
      | Ok (st, _) -> Fmt.error "%a" pp_cmd_status (cmd, st)
      | Error _ as e -> e

    (* Non-blocking command *)

    type pid = int
    let pid_to_int pid = pid

    let spawn
        ?env ?cwd ?(stdin = in_stdin) ?(stdout = out_stdout)
        ?(stderr = out_stderr) cmd
      =
      let fds = Fd.Set.empty () in
      try Ok (_spawn fds ?env ?cwd ~stdin ~stdout ~stderr cmd) with
      | Failure e -> Fd.Set.close_all fds; spawn_err cmd e
      | Unix.Unix_error (e, _, _) ->
          Fd.Set.close_all fds; spawn_err cmd (uerr e)

    let rec spawn_poll_status pid = match Unix.waitpid Unix.[WNOHANG] pid with
    | 0, _ -> Ok None
    | _, status -> Ok (Some (status_of_unix_status status))
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> spawn_poll_status pid
    | exception Unix.Unix_error (e, _, _) ->
        Fmt.error "poll_status: waitpid %d: %s" pid (uerr e)

    let rec spawn_wait_status pid = match Unix.waitpid [] pid with
    | _, status -> Ok (status_of_unix_status status)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> spawn_wait_status pid
    | exception Unix.Unix_error (e, _, _) ->
        Fmt.error "wait_status: waitpid %d: %s" pid (uerr e)

    (* execv

       On Windows when Unix.execv[e] is invoked, control is returned to
       the controlling terminal when the child process starts (vs. child
       process terminates on POSIX). This entails all sort of weird
       behaviour. To workaround this, our execv[e] on Windows simply
       runs the program as a sub-process on which we waitpid(2) and then
       exit with the resulting status. *)

    let _execv_win32 ~env f cmd =
      let exit pid = match Unix.waitpid [] pid with
      | _, (Unix.WEXITED c) -> exit c
      | _, (Unix.WSIGNALED sg) ->
          Unix.(kill (getpid ()) sg);
          (* In case we don't get killed, exit with bash convention. *)
          exit (128 + sg)
      | _ -> assert false
      in
      let env = spawn_env env in
      exit Unix.(create_process_env f cmd env stdin stderr stderr)

    let _execv_posix ~env f cmd =
      Ok (Unix.execve f cmd (spawn_env env))
    let _execv = if Sys.win32 then _execv_win32 else _execv_posix

    let execv ?env ?cwd f cmd =
      let err_execv f e = Fmt.error "execv %a: %s" Fpath.pp f e in
      try
        Option.iter chdir cwd;
        !_spawn_tracer None env cwd cmd;
        _execv ~env (Fpath.to_string f) (Array.of_list @@ Cmd.to_list cmd)
      with
      | Failure e -> err_execv f e
      | Unix.Unix_error (e, _, _) -> err_execv f (uerr e)
  end
end

module Log = struct

  (* Reporting levels *)

  type level = Quiet | App | Error | Warning | Info | Debug
  let _level = ref Warning
  let level () = !_level
  let set_level l = _level := l
  let level_to_string = function
  | Quiet -> "quiet" | App -> "app" | Error -> "error" | Warning -> "warning"
  | Info -> "info" | Debug -> "debug"

  let level_of_string = function
  | "quiet" -> Ok Quiet
  | "app" -> Ok App
  | "error" -> Ok Error
  | "warning" -> Ok Warning
  | "info" -> Ok Info
  | "debug" -> Ok Debug
  | l -> Fmt.error "%a: unknown log level" Fmt.(quote string) l

  (* Reporting *)

  let app_style = [`Fg `Cyan]
  let err_style = [`Fg `Red]
  let warn_style = [`Fg `Yellow]
  let info_style = [`Fg `Blue]
  let debug_style = [`Faint; `Fg `Magenta]

  let pp_level_str level ppf v = match level with
  | App -> Fmt.tty_string app_style ppf v
  | Error -> Fmt.tty_string err_style ppf v
  | Warning -> Fmt.tty_string warn_style ppf v
  | Info -> Fmt.tty_string info_style ppf v
  | Debug -> Fmt.tty_string debug_style ppf v
  | Quiet -> assert false

  let pp_level ppf level = match level with
  | App -> ()
  | Error -> Fmt.tty_string err_style ppf "ERROR"
  | Warning -> Fmt.tty_string warn_style ppf "WARNING"
  | Info -> Fmt.tty_string info_style ppf "INFO"
  | Debug -> Fmt.tty_string debug_style ppf "DEBUG"
  | Quiet -> assert false

  let pp_header =
    let x = match Array.length Sys.argv with
    | 0 -> Filename.basename Sys.executable_name
    | n -> Filename.basename Sys.argv.(0)
    in
    let pp_header ppf (l, h) = match h with
    | None -> if l = App then () else Fmt.pf ppf "%s: [%a] " x pp_level l
    | Some "" -> ()
    | Some h -> Fmt.pf ppf "%s: [%a] " x (pp_level_str l) h
    in
    pp_header

  (* Log functions *)

  let _err_count = ref 0
  let err_count () = !_err_count
  let incr_err_count () = incr _err_count

  let _warn_count = ref 0
  let warn_count () = !_warn_count
  let incr_warn_count () = incr _warn_count

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
      if level = App then Format.std_formatter else Format.err_formatter
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
  let app msgf = !_kmsg.kmsg kunit App msgf
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

  let warn_if_error ?header ~use r = if_error ~level:Warning ?header ~use r

  let if_error_pp ?(level = Error) ?header pp ~use = function
  | Ok v -> v
  | Error e ->
      !_kmsg.kmsg (fun _ -> use) level @@ fun m ->
      m ?header "@[%a@]" pp e

  (* Logging timings *)

  let time ?(level = Info) m f =
    let time = Time.counter () in
    let r = f () in
    let span = Time.count time in
    !_kmsg.kmsg (fun () -> r) level
      (fun w ->
         let header = Format.asprintf "%a" Time.Span.pp span in
         m r (w ~header))
end

(* Value converters *)

module Conv = struct

  (* Encoders and decoders *)

  exception Error of int * int * string
  let err i fmt = Fmt.kstr (fun e -> raise_notrace @@ Error (i, i, e)) fmt

  module Bin = struct

    (* Encoding *)

    type 'a enc = Buffer.t -> 'a -> unit

    let enc_err ~kind msg = err 0 ("encoding %s: " ^^ msg) kind
    let enc_check_int ~kind (min : int) (max : int) (v : int) =
      if min <= v && v <= max then () else
      enc_err ~kind "%d: not in range [%d;%d]" v min max

    let enc_byte b byte = Buffer.add_char b (Char.unsafe_chr byte)
    let enc_int32 b i =
      (* XXX From 4.08 use Buffer.add_int32_le *)
      let i0 = Int32.to_int i in
      let i1 = Int32.to_int (Int32.shift_right_logical i 16) in
      let b0 = i0 land 0xFF and b1 = (i0 lsr 8) land 0xFF
      and b2 = i1 land 0xFF and b3 = (i1 lsr 8) land 0xFF in
      enc_byte b b0; enc_byte b b1; enc_byte b b2; enc_byte b b3

    let enc_int64 b i =
      (* XXX From 4.08 one use Buffer.add_int64_le *)
      let i0 = Int64.to_int i in
      let i1 = Int64.to_int (Int64.shift_right_logical i 16) in
      let i2 = Int64.to_int (Int64.shift_right_logical i 32) in
      let i3 = Int64.to_int (Int64.shift_right_logical i 48) in
      let b0 = i0 land 0xFF and b1 = (i0 lsr 8) land 0xFF
      and b2 = i1 land 0xFF and b3 = (i1 lsr 8) land 0xFF
      and b4 = i2 land 0xFF and b5 = (i2 lsr 8) land 0xFF
      and b6 = i3 land 0xFF and b7 = (i3 lsr 8) land 0xFF in
      enc_byte b b0; enc_byte b b1; enc_byte b b2; enc_byte b b3;
      enc_byte b b4; enc_byte b b5; enc_byte b b6; enc_byte b b7

    let enc_int b v = enc_int64 b (Int64.of_int v)
    let enc_len b v = enc_int64 b (Int64.of_int v)

    let enc_bytes b v =
      enc_len b (String.length v); Buffer.add_string b v

    let enc_list enc_v b vs =
      let rec loop len acc = function
      | [] -> len, acc | v :: vs -> loop (len + 1) (v :: acc) vs
      in
      let len, rvs = loop 0 [] vs in
      enc_len b len; List.iter (enc_v b) rvs

    (* Decoding *)

    type 'a dec = string -> start:int -> int * 'a

    let dec_err ~kind i msg = err i ("decoding %s: " ^^ msg) kind
    let dec_err_eoi ~kind i = dec_err ~kind i "unexpected end of input"
    let dec_err_exceed ~kind i v ~max =
      dec_err ~kind i "%d: not in range [0;%d]" v max

    let dec_need ~kind s ~start ~len =
      match start + (len - 1) < String.length s with
      | true -> () | false -> dec_err_eoi ~kind start

    let dec_byte ~kind s ~start =
      dec_need ~kind s ~start ~len:1; start + 1, Char.code (s.[start])

    external swap32 : int32 -> int32 = "%bswap_int32"
    external unsafe_get_int32_ne : string -> int -> int32 =
      "%caml_string_get32u"

    let unsafe_get_int32_le s i = match Sys.big_endian with
    | true -> swap32 (unsafe_get_int32_ne s i)
    | false -> unsafe_get_int32_ne s i

    let dec_int32 ~kind s ~start =
      let len = 4 in
      dec_need ~kind s ~start ~len; start + len, unsafe_get_int32_le s start

    external swap64 : int64 -> int64 = "%bswap_int64"
    external unsafe_get_int64_ne : string -> int -> int64 =
      "%caml_string_get64u"

    let unsafe_get_int64_le b i = match Sys.big_endian with
    | true -> swap64 (unsafe_get_int64_ne b i)
    | false -> unsafe_get_int64_ne b i

    let dec_int64 ~kind s ~start =
      let len = 8 in
      dec_need ~kind s ~start ~len; start + len, unsafe_get_int64_le s start

    let int_min = Int64.of_int min_int
    let int_max = Int64.of_int max_int
    let int_len = 8
    let dec_int ~kind s ~start =
      let i, v = dec_int64 ~kind s ~start in
      if (Int64.compare int_min v <= 0 && Int64.compare v int_max <= 0)
      then i, Int64.to_int v
      else dec_err ~kind start "%Ld: not in range [%Ld;%Ld]" v int_min int_max

    let len_max = Int64.of_int max_int
    let len_len = 8
    let dec_len ~kind s ~start =
      let i, l = dec_int64 ~kind s ~start in
      if (Int64.compare 0L l <= 0 && Int64.compare l len_max <= 0)
      then i, Int64.to_int l
      else dec_err ~kind start "%Ld: not in range [0;%Ld]" l len_max

    let dec_bytes ~kind s ~start =
      let start, len = dec_len ~kind s ~start in
      let last = start + len - 1 in
      dec_need ~kind s ~start ~len;
      start + len, String.with_index_range ~first:start ~last s

    let dec_list dec_v ~kind s ~start =
      let start, len = dec_len ~kind s ~start in
      let rec loop acc count start = match count < 1 with
      | true -> start, acc
      | false ->
          let start, v = dec_v s ~start in
          loop (v :: acc) (count - 1) start
      in
      loop [] len start
  end

  module Txt = struct

    let is_atom_sep = function
    | '(' | ')' | ';' | '"' -> true
    | c when Char.Ascii.is_white c -> true
    | c -> false

    (* Encoding *)

    type 'a enc = Format.formatter -> 'a -> unit

    let enc_err ~kind msg = err 0 ("encoding %s: " ^^ msg) kind
    let enc_list enc_v ppf vs =
      Fmt.pf ppf "@[<1>(%a)@]" Fmt.(list ~sep:sp enc_v) vs

    let enc_atom ppf a =
      let atom_must_quote a =
        let rec loop max i s = match i < max with
        | false -> if max < 0 then true (* empty string *) else false
        | true ->
            match a.[i] with
            | '^' -> true
            | c when is_atom_sep c -> true
            | c when Char.Ascii.is_control c -> true
            | c -> loop max (i + 1) s
        in
        loop (String.length a - 1) 0 a
      in
      match atom_must_quote a with
      | false -> Fmt.string ppf a
      | true ->
          let len = String.length a in
          let flush ppf start i =
            if start < len then Fmt.string ppf (String.sub a start (i - start))
          in
          let rec loop start i = match i < len with
          | false -> flush ppf start i
          | true ->
              let next = i + 1 in
              match a.[i] with
              | '"' -> flush ppf start i; Fmt.string ppf "^\""; loop next next
              | '^' -> flush ppf start i; Fmt.string ppf "^^"; loop next next
              | '\n' -> flush ppf start i; Fmt.string ppf "^n"; loop next next
              | '\r' -> flush ppf start i; Fmt.string ppf "^r"; loop next next
              | c when Char.Ascii.is_control c ->
                  flush ppf start i; Fmt.pf ppf "^u{%04X}" (Char.code c);
                  loop next next
              | c -> loop start next
          in
          Fmt.char ppf '\"'; loop 0 0; Fmt.char ppf '\"'

    let enc_check_int ~kind (min : int) (max : int) (v : int) =
      if min <= v && v <= max then () else
      enc_err ~kind "%d: not in range [%d;%d]" v min max

    (* Decoding *)

    type 'a dec = string -> start:int -> int * 'a

    let err_char c = String.Ascii.escape (String.of_char c)
    let dec_err ~kind i msg = err i ("parsing %s: " ^^ msg) kind
    let dec_err_eoi ~kind i = dec_err ~kind i "unexpected end of input"
    let dec_err_char ~kind i c =
      dec_err ~kind i "%s: illegal character" (err_char c)

    type lexeme = [ `Ls |  `Le | `Atom of string ]
    let pp_lexeme ppf = function
    | `Ls -> Fmt.string ppf "\"(\""
    | `Le -> Fmt.string ppf "\")\""
    | `Atom s -> Fmt.pf ppf "\"%s\"" s

    let dec_err_lexeme ~kind i l ~exp =
      dec_err ~kind i "%a: expected %a" pp_lexeme l
        Fmt.(list ~sep:(any " or ") pp_lexeme) exp

    let dec_err_atom ~kind i a ~exp =
      let exp = List.map (fun s -> Fmt.str "\"%s\"" s) exp in
      dec_err ~kind i "%s: expected %a"
        a Fmt.(list ~sep:(any " or ") string) exp

    let dec_err_unclosed ~kind i = dec_err ~kind i "unclosed list"
    let dec_err_esc_truncated ~kind i = dec_err ~kind i "truncated escape"

    let char_is_illegal = function
    | '\x00' .. '\x08' | '\x0E' .. '\x1F' | '\x7F' -> true
    | _ -> false

    let dec_skip ~kind s ~start =
      let rec skip len i =
        if i >= len then len else
        match s.[i] with
        | ';' -> skip_comment len (i + 1)
        | c when Char.Ascii.is_white s.[i] -> skip len (i + 1)
        | c when char_is_illegal c -> dec_err_char ~kind i c
        | c -> i
      and skip_comment len i =
        if i >= len then len else
        match s.[i] with
        | '\n' | '\r' -> skip len (i + 1)
        | c when char_is_illegal c -> dec_err_char ~kind i c
        | c -> skip_comment len (i + 1)
      in
      skip (String.length s) start

    (* XXX From 4.06 use Buffer.add_utf_8_uchar *)
    let buffer_add_utf_8_uchar b u = match Uchar.to_int u with
    | u when u < 0 -> assert false
    | u when u <= 0x007F ->
        Buffer.add_char b (Char.unsafe_chr u)
    | u when u <= 0x07FF ->
        Buffer.add_char b (Char.unsafe_chr (0xC0 lor (u lsr 6)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (u land 0x3F)));
    | u when u <= 0xFFFF ->
        Buffer.add_char b (Char.unsafe_chr (0xE0 lor (u lsr 12)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (u land 0x3F)));
    | u when u <= 0x10FFFF ->
        Buffer.add_char b (Char.unsafe_chr (0xF0 lor (u lsr 18)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((u lsr 12) land 0x3F)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (u land 0x3F)))
    | _ -> assert false

    let dec_uchar_esc ~kind s ~start i =
      let max = String.length s - 1 in
      if i > max then dec_err_esc_truncated ~kind start else
      match s.[i] <> '{' with
      | true ->
          dec_err ~kind i "^u%s: illegal escape sequence" (err_char s.[i])
      | false ->
          let i = i + 1 in
          let rec loop acc count = match i + count with
          | k when k > max -> dec_err_esc_truncated ~kind start
          | k ->
              match s.[k] with
              | c when Char.Ascii.is_hex_digit c ->
                  let count = count + 1 in
                  if count > 6
                  then dec_err ~kind k "%c: expected \"}\"" c else
                  let acc = acc * 16 + Char.Ascii.hex_digit_value c in
                  loop acc count
              | '}' ->
                  if count = 0
                  then dec_err ~kind k "}: expected hex digit"
                  else if not (Uchar.is_valid acc)
                  then dec_err ~kind start
                      "%04X: Unicode escape is not a Unicode scalar value" acc
                  else (k + 1, Uchar.of_int acc)
              | c ->
                  dec_err ~kind k
                    "%s: expected hex digit" (err_char c)
          in
          loop 0 0

    let dec_qatom ~kind s ~start:astart = (* '\"' at [astart] was parsed *)
      let len = String.length s in
      let b = Buffer.create 255 in
      let flush start i =
        if start < len then Buffer.add_substring b s start (i - start)
      in
      let rec loop start i = match i < len with
      | false -> dec_err ~kind astart "unclosed quoted atom"
      | true ->
          match s.[i] with
          | '\"' -> flush start i; i + 1, Buffer.contents b
          | '^' -> (* totally evil escape sequence *)
              flush start i;
              let esc = i + 1 in
              let next = i + 2 in
              if esc < len then () else dec_err_esc_truncated ~kind i;
              begin match s.[esc] with
              | '\"' -> Buffer.add_char b '\"'; loop next next
              | '^' -> Buffer.add_char b '^'; loop next next
              | 'n' -> Buffer.add_char b '\n'; loop next next
              | 'r' -> Buffer.add_char b '\r'; loop next next
              | ' ' -> Buffer.add_char b ' '; loop next next
              | 'u' ->
                  let i, u = dec_uchar_esc ~kind s ~start:i next in
                  buffer_add_utf_8_uchar b u; loop i i
              | '\n' | '\r' -> (* continuation line *)
                  let rec skip_white s i = match i < len with
                  | false -> i
                  | true when char_is_illegal s.[i] ->
                      dec_err_char ~kind i s.[i]
                  | true when Char.Ascii.is_white s.[i] -> skip_white s (i + 1)
                  | true -> i
                  in
                  let start = skip_white s next in
                  loop start start
              | c ->
                  dec_err ~kind i "^%s: illegal escape sequence" (err_char c)
              end
          | c when char_is_illegal c -> dec_err_char ~kind i c
          | c -> loop start (i + 1)
      in
      let start = astart + 1 in
      loop start start

    let dec_atom ~kind s ~start =
      let i = dec_skip ~kind s ~start in
      match i < String.length s with
      | false -> dec_err ~kind i "end of input, expected atom"
      | true ->
          if s.[i] = '\"' then dec_qatom ~kind s ~start:i else
          let rec loop max i s = match i > max with
          | true -> i - 1
          | false ->
              match s.[i] with
              | c when char_is_illegal c -> dec_err_char ~kind i c
              | c when is_atom_sep c -> i - 1
              | c -> loop max (i + 1) s
          in
          let last = loop (String.length s - 1) (i + 1) s in
          let atom = String.with_index_range ~first:i ~last s in
          last + 1, atom

    let dec_lx par ~kind s ~start =
      let i = dec_skip ~kind s ~start in
      match i < String.length s with
      | false -> dec_err ~kind i "end of input, expected \"%c\" " par
      | true when Char.equal s.[i] par -> i + 1
      | true ->
          dec_err ~kind start "%s: expected \"%c\"" (err_char s.[i]) par

    let dec_ls ~kind s ~start = dec_lx '(' ~kind s ~start
    let dec_le ~kind s ~start = dec_lx ')' ~kind s ~start
    let dec_lexeme ~kind s ~start =
      let i = dec_skip ~kind s ~start in
      match i < String.length s with
      | false -> dec_err_eoi ~kind i
      | true ->
          match s.[i] with
          | '(' -> i + 1, (i, `Ls)
          | ')' -> i + 1, (i, `Le)
          | _ -> let j, a = dec_atom ~kind s ~start:i in j, (i, `Atom a)

    let dec_t_of_atom f ~kind s ~start:astart =
      let i, a = dec_atom ~kind s ~start:astart in
      match f a with
      | v -> i, v
      | exception Failure _ -> dec_err ~kind astart "%s: not a literal" a

    let dec_list dec_v ~kind s ~start =
      let lstart = dec_ls ~kind s ~start in
      let rec loop acc start =
        let i = dec_skip ~kind s ~start in
        match i < String.length s with
        | false -> dec_err_unclosed ~kind lstart
        | true ->
            if s.[i] = ')' then dec_le ~kind s ~start:i, List.rev acc else
            let start, v = dec_v s ~start:i in
            loop (v :: acc) start
      in
      loop [] lstart

    let dec_list_head ~kind s ~start =
      let start = dec_ls ~kind s ~start in
      dec_atom ~kind s ~start

    let dec_list_tail dec_v ~kind ~ls s ~start =
      let rec loop acc i =
        let i = dec_skip ~kind s ~start:i in
        match i < String.length s with
        | false -> dec_err_unclosed ~kind ls
        | true ->
            if s.[i] = ')' then dec_le ~kind s ~start:i, List.rev acc else
            let i, v = dec_v s ~start:i in
            loop (v :: acc) start
      in
      loop [] start

    let dec_check_int ~kind i (min : int) (max : int) (v : int) =
      if min <= v && v <= max then () else
      dec_err ~kind i "%d: not in range [%d;%d]" v min max
  end

  (* Converters *)

  type 'a t =
    { kind : string; docvar : string;
      bin_enc : 'a Bin.enc; bin_dec : 'a Bin.dec;
      txt_enc : 'a Txt.enc; txt_dec : 'a Txt.dec }

  let v ~kind ~docvar bin_enc bin_dec txt_enc txt_dec =
    { kind; docvar; bin_enc; bin_dec; txt_enc; txt_dec }

  let kind c = c.kind
  let docvar c = c.docvar
  let bin_enc c = c.bin_enc
  let bin_dec c = c.bin_dec
  let txt_enc c = c.txt_enc
  let txt_dec c = c.txt_dec
  let with_kind ?docvar kind c =
    let docvar = match docvar with None -> c.docvar | Some v -> v in
    { c with kind; docvar }

  let with_docvar docvar c = { c with docvar }
  let with_conv ~kind ~docvar to_t of_t conv_t =
    let bin_enc b v = conv_t.bin_enc b (to_t v) in
    let bin_dec s ~start = let i, v = conv_t.bin_dec s ~start in i, of_t v in
    let txt_enc ppf v = conv_t.txt_enc ppf (to_t v) in
    let txt_dec s ~start = let i, v = conv_t.bin_dec s ~start in i, of_t v in
    { kind; docvar; bin_enc; bin_dec; txt_enc; txt_dec }

  (* Converting *)

  let get_buf = function None -> Buffer.create 256 | Some b -> Buffer.clear b; b

  let to_bin ?buf c v =
    let b = get_buf buf in
    try Ok (c.bin_enc b v; Buffer.contents b)
    with Error (_, _, e) -> Result.Error e

  let of_bin c s =
    try
      let i, v = c.bin_dec s 0 in
      match i = String.length s with
      | true -> Ok v
      | false -> Bin.dec_err ~kind:c.kind i "unexpected leftover bytes"
    with Error (i, _, e) -> Fmt.error "%d: %s" i e

  let to_txt ?buf c v =
    let b = get_buf buf in
    let ppf = Format.formatter_of_buffer b in
    try Ok (c.txt_enc ppf v; Format.pp_print_flush ppf (); Buffer.contents b)
    with Error (_, _, e) -> Result.Error e

  let of_txt c s =
    try
      let i, v = c.txt_dec s ~start:0 in
      match i = String.length s with
      | true -> Ok v
      | false ->
          match Txt.dec_skip ~kind:c.kind s ~start:i = String.length s with
          | true -> Ok v
          | false -> Txt.dec_err ~kind:c.kind i "unexpected leftover text"
    with Error (i, _, e) -> Fmt.error "%d: %s" i e

  let to_pp c ppf v = try c.txt_enc ppf v with
  | Error (_, _, e) ->
      Fmt.pf ppf "@[<1>(conv-error %a %a)@]" Txt.enc_atom c.kind Txt.enc_atom e

  (* Predefined converters *)

  let unit =
    let kind = "unit" in
    let bin_enc b v = Bin.enc_byte b 0 in
    let bin_dec s ~start = match Bin.dec_byte ~kind s ~start with
    | i, 0 -> i, ()
    | i, byte -> Bin.dec_err ~kind i "expected 0"
    in
    let txt_enc ppf v = Fmt.string ppf "unit" in
    let txt_dec s ~start =
      let i, a = Txt.dec_atom ~kind s ~start in
      match a with
      | "unit" -> i, ()
      | a -> Txt.dec_err_atom ~kind start a ~exp:["unit"]
    in
    v ~kind ~docvar:"UNIT" bin_enc bin_dec txt_enc txt_dec

  let bool =
    let kind = "bool" in
    let bin_enc b v = Bin.enc_byte b (if v then 1 else 0) in
    let bin_dec s ~start = match Bin.dec_byte ~kind s ~start with
    | i, 0 -> i, false
    | i, 1 -> i, true
    | _, byte -> Bin.dec_err_exceed ~kind start byte ~max:1
    in
    let txt_enc ppf v = Fmt.string ppf (if v then "true" else "false") in
    let txt_dec s ~start =
      let i, a = Txt.dec_atom ~kind s ~start in
      let v = match a with
      | "true" -> true | "false" -> false
      | a -> Txt.dec_err_atom ~kind start a ~exp:["true"; "false"]
      in
      i, v
    in
    v ~kind ~docvar:"BOOL" bin_enc bin_dec txt_enc txt_dec

  let byte =
    let kind = "byte" in
    let min = 0 and max = 255 in
    let bin_enc b v = Bin.enc_check_int ~kind min max v; Bin.enc_byte b v in
    let bin_dec = Bin.dec_byte ~kind in
    let txt_enc ppf v = Txt.enc_check_int ~kind min max v; Fmt.int ppf v in
    let txt_dec s ~start =
      let i, v as r = Txt.dec_t_of_atom int_of_string ~kind s ~start in
      Txt.dec_check_int ~kind start min max v; r
    in
    v ~kind ~docvar:"BYTE" bin_enc bin_dec txt_enc txt_dec

  let int =
    let kind = "int" in
    let bin_enc = Bin.enc_int in
    let bin_dec = Bin.dec_int ~kind in
    let txt_enc = Fmt.int in
    let txt_dec s ~start = Txt.dec_t_of_atom int_of_string ~kind s ~start in
    v ~kind ~docvar:"INT" bin_enc bin_dec txt_enc txt_dec

  let int31 =
    let kind = "int31" in
    let min = - (1 lsl 30) and max = 1 lsl 30 - 1 in
    let bin_enc b v =
      Bin.enc_check_int ~kind min max v;
      let b0 = v land 0xFF          and b1 = (v lsr 8) land 0xFF
      and b2 = (v lsr 16) land 0xFF and b3 = (v lsr 24) land 0xFF in
      Bin.enc_byte b b0; Bin.enc_byte b b1; Bin.enc_byte b b2; Bin.enc_byte b b3
    in
    let bin_dec s ~start =
      let len = 4 in
      Bin.dec_need ~kind s ~start ~len;
      let b0 = Char.code s.[start]     and b1 = Char.code s.[start + 1]
      and b2 = Char.code s.[start + 2] and b3 = Char.code s.[start + 3] in
      let max_bit = Sys.int_size in
      let i =
        (b3 lsl (max_bit - 7))  lor (b2 lsl (max_bit - 15)) lor
        (b1 lsl (max_bit - 23)) lor (b0 lsl (max_bit - 31))
      in
      let i = i asr (max_bit - 31) in
      start + len, i
    in
    let txt_enc ppf v = Txt.enc_check_int ~kind min max v; Fmt.int ppf v in
    let txt_dec s ~start =
      let (i, v as r) = Txt.dec_t_of_atom  int_of_string ~kind s ~start in
      Txt.dec_check_int ~kind start min max v; r
    in
    v ~kind ~docvar:"INT31" bin_enc bin_dec txt_enc txt_dec

  let int32 =
    let kind = "int32" in
    let bin_enc = Bin.enc_int32 in
    let bin_dec = Bin.dec_int32 ~kind in
    let txt_enc = Fmt.int32 in
    let txt_dec s ~start = Txt.dec_t_of_atom Int32.of_string ~kind s ~start in
    v ~kind ~docvar:"INT32" bin_enc bin_dec txt_enc txt_dec

  let int64 =
    let kind = "int64" in
    let bin_enc = Bin.enc_int64 in
    let bin_dec = Bin.dec_int64 ~kind in
    let txt_enc = Fmt.int64 in
    let txt_dec s ~start = Txt.dec_t_of_atom Int64.of_string ~kind s ~start in
    v ~kind ~docvar:"INT64" bin_enc bin_dec txt_enc txt_dec

  let float =
    let kind = "float" in
    let bin_enc b v = Bin.enc_int64 b (Int64.bits_of_float v) in
    let bin_dec s ~start =
      let i, v = Bin.dec_int64 ~kind s ~start in i, Int64.float_of_bits v
    in
    let txt_enc = Fmt.float in
    let txt_dec s ~start = Txt.dec_t_of_atom float_of_string ~kind s ~start in
    v ~kind ~docvar:"FLOAT" bin_enc bin_dec txt_enc txt_dec

  let string_bytes =
    let kind = "string" in
    let bin_enc = Bin.enc_bytes in
    let bin_dec = Bin.dec_bytes ~kind in
    let txt_enc ppf = function
    | "" -> Fmt.pf ppf "@[<1>(hex@ \"\")@]"
    | v -> Fmt.pf ppf "@[<1>(hex@ %s)@]" (String.Ascii.to_hex v)
    in
    let txt_dec s ~start =
      let astart = Txt.dec_ls ~kind s ~start in
      let start, a = Txt.dec_atom ~kind s ~start:astart in
      if a <> "hex" then Txt.dec_err ~kind astart "%s: expected \"hex\"" a else
      let start, h = Txt.dec_atom ~kind s ~start in
      match String.Ascii.of_hex h with
      | Ok v -> Txt.dec_le ~kind s ~start, v
      | Error i when i = String.length s ->
          Txt.dec_err ~kind (start + i) "missing hex digit"
      | Error i ->
          Txt.dec_err ~kind (start + i) "not a hex digit"
    in
    v ~kind ~docvar:"HEX" bin_enc bin_dec txt_enc txt_dec

  let atom =
    let kind = "atom" in
    let bin_enc = Bin.enc_bytes in
    let bin_dec = Bin.dec_bytes ~kind in
    let txt_enc = Txt.enc_atom in
    let txt_dec = Txt.dec_atom ~kind in
    v ~kind ~docvar:"VALUE" bin_enc bin_dec txt_enc txt_dec

  let atom_non_empty =
    let kind = "atom" in
    let bin_enc b = function
    | "" -> Bin.enc_err ~kind "empty atom" | v -> Bin.enc_bytes b v
    in
    let bin_dec s ~start =
      let (i, v as r) = Bin.dec_bytes ~kind s ~start in
      if v = "" then Bin.dec_err ~kind start "empty atom" else r
    in
    let txt_enc ppf = function
    | "" -> Txt.enc_err ~kind "empty atom" | v -> Txt.enc_atom ppf v
    in
    let txt_dec s ~start =
      let (i, v as r) = Txt.dec_atom ~kind s ~start in
      if v = "" then Txt.dec_err ~kind start "empty atom" else r
    in
    v ~kind ~docvar:"VALUE" bin_enc bin_dec txt_enc txt_dec

  let fpath =
    let kind = "path" in
    let bin_enc b v = Bin.enc_bytes b (Fpath.to_string v) in
    let bin_dec s ~start =
      let i, v = Bin.dec_bytes ~kind s ~start in
      match Fpath.of_string v with
      | Error e -> Bin.dec_err ~kind start "%s" e
      | Ok v -> i, v
    in
    let txt_enc ppf v = Txt.enc_atom ppf (Fpath.to_string v) in
    let txt_dec s ~start =
      let i, v = Txt.dec_atom ~kind s ~start in
      match Fpath.of_string v with
      | Error e -> Txt.dec_err ~kind start "%s" e
      | Ok v -> i, v
    in
    v ~kind ~docvar:"PATH" bin_enc bin_dec txt_enc txt_dec

  let hash = with_kind "hash" ~docvar:"HASH" string_bytes
  let time_span =
    let kind = "time span" in
    let bin_enc = Bin.enc_int64 in
    let bin_dec = Bin.dec_int64 ~kind in
    let txt_enc ppf v = Fmt.pf ppf "0u%Lu" v in
    let txt_dec = Txt.dec_t_of_atom Int64.of_string ~kind in
    v ~kind ~docvar:"TIMESPAN" bin_enc bin_dec txt_enc txt_dec

  let time_cpu_span =
    let kind = "cpu span" in
    let bin_enc b v =
      Bin.enc_int64 b (Int64.bits_of_float v.Unix.tms_utime);
      Bin.enc_int64 b (Int64.bits_of_float v.Unix.tms_stime);
      Bin.enc_int64 b (Int64.bits_of_float v.Unix.tms_cutime);
      Bin.enc_int64 b (Int64.bits_of_float v.Unix.tms_cstime);
    in
    let bin_dec s ~start =
      Bin.dec_need ~kind s ~start ~len:32;
      let start, ut = Bin.dec_int64 ~kind s ~start in
      let start, st = Bin.dec_int64 ~kind s ~start in
      let start, cut = Bin.dec_int64 ~kind s ~start in
      let start, cst = Bin.dec_int64 ~kind s ~start in
      let f i = Int64.float_of_bits i in
      start, Unix.{ tms_utime = f ut; tms_stime = f st; tms_cutime = f cut;
                    tms_cstime = f cst }
    in
    let txt_enc ppf = Txt.enc_err ~kind "Unsupported" in
    let txt_dec s ~start = Txt.dec_err ~kind start "Unsupported" in
    v ~kind ~docvar:"CPU-SPAN" bin_enc bin_dec txt_enc txt_dec

  let cmd = (* XXX not tail recursive *)
    let kind = "cmd" in
    let rec bin_enc b = function
    | Cmd.A a -> Bin.enc_byte b 0x0; Bin.enc_bytes b a
    | Cmd.Rseq l -> Bin.enc_byte b 0x1; Bin.enc_list bin_enc b l
    | Cmd.Shield c -> Bin.enc_byte b 0x2; bin_enc b c
    in
    let rec bin_dec s ~start = match Bin.dec_byte ~kind s ~start with
    | start, 0x0 -> let i, a = Bin.dec_bytes ~kind s ~start in i, Cmd.A a
    | start, 0x1 ->
        let i, l = Bin.dec_list bin_dec ~kind s ~start in i, Cmd.Rseq l
    | start, 0x2 -> let i, c = bin_dec s ~start in i, Cmd.Shield c
    | _, byte -> Bin.dec_err_exceed ~kind start byte ~max:0x2
    in
    let rec txt_enc ppf = function
    | Cmd.A a -> Txt.enc_atom ppf a
    | Cmd.Rseq l -> Txt.enc_list txt_enc ppf (Cmd.A "rseq" :: l)
    | Cmd.Shield c -> Txt.enc_list txt_enc ppf (Cmd.A "shield" :: c :: [])
    in
    let rec txt_dec s ~start = match Txt.dec_lexeme ~kind s ~start with
    | i, (_, `Atom a) -> i, Cmd.A a
    | i, (ls, `Ls) ->
        let i, a = Txt.dec_atom ~kind s ~start:i in
        begin match a with
        | "rseq" ->
            let i, l = Txt.dec_list_tail txt_dec ~kind ~ls s ~start:i in
            start, Cmd.Rseq l
        | "shield" ->
            let i, c = txt_dec s ~start:i in
            let i = Txt.dec_le ~kind s ~start:i in
            i, Cmd.Shield c
        | a ->
            Txt.dec_err_atom ~kind i a ~exp:["rseq"; "shield"]
        end
    | _, (j, l) ->
        Txt.dec_err_lexeme ~kind j l ~exp:[`Ls; `Atom "atom" (* bof *)]
    in
    v ~kind ~docvar:"CMD" bin_enc bin_dec txt_enc txt_dec

  let os_cmd_status : Os.Cmd.status t =
    let kind = "cmd-status" in
    let bin_enc b = function
    | `Exited e -> Bin.enc_byte b 0x0; Bin.enc_byte b e
    | `Signaled s -> Bin.enc_byte b 0x1; Bin.enc_byte b (- s)
    in
    let bin_dec s ~start = match Bin.dec_byte ~kind s ~start with
    | start, 0x0 ->
        let i, v = Bin.dec_byte s ~kind ~start in i, `Exited v
    | start, 0x1 ->
        let i, v = Bin.dec_byte s ~kind ~start in i, `Signaled (- v)
    | _, byte ->
        Bin.dec_err_exceed ~kind start byte ~max:0x1
    in
    let txt_enc ppf = raise (Error (0, 0, "TODO Not implemented")) in
    let txt_dec s ~start = raise (Error (0, 0, "TODO Not implemented")) in
    v ~kind ~docvar:"STATUS" bin_enc bin_dec txt_enc txt_dec

  (* Higher-order converters *)

  let option ?kind:k ?docvar:d c =
    let kind = match k with Some k -> k | None -> Fmt.str "%s option" c.kind in
    let docvar = match d with Some v -> v | None -> Fmt.str "[%s]" c.docvar in
    let bin_enc b = function
    | None -> Bin.enc_byte b 0x0
    | Some v -> Bin.enc_byte b 0x1; c.bin_enc b v
    in
    let bin_dec s ~start = match Bin.dec_byte ~kind s ~start with
    | start, 0x0 -> start, None
    | start, 0x1 -> let i, v = c.bin_dec s ~start in i, Some v
    | _, byte -> Bin.dec_err_exceed ~kind start byte ~max:0x1
    in
    let txt_enc ppf = function
    | None -> Fmt.pf ppf "none"
    | Some v -> Fmt.pf ppf "@[<1>(some@ @[%a@])@]" c.txt_enc v
    in
    let txt_dec s ~start = match Txt.dec_lexeme ~kind s ~start with
    | i, (j, `Ls) ->
        let start, a = Txt.dec_atom ~kind s ~start:i in
        if String.equal a "some" then
          let start, v = c.txt_dec s ~start in
          Txt.dec_le ~kind s ~start, Some v
        else
          Txt.dec_err_atom ~kind j a ~exp:["some"]
    | i, (_, `Atom "none") -> i, None
    | _, (j, l) -> Txt.dec_err_lexeme ~kind j l ~exp:[`Ls; `Atom "none"]
    in
    v ~kind ~docvar bin_enc bin_dec txt_enc txt_dec

  let some c =
    let err_none = format_of_string "cannot encode None" in
    let bin_enc b = function
    | None -> Bin.enc_err ~kind:c.kind err_none
    | Some v -> c.bin_enc b v
    in
    let bin_dec s ~start = let i, v = c.bin_dec s ~start in i, Some v in
    let txt_enc ppf = function
    | None -> Txt.enc_err ~kind:c.kind err_none
    | Some v -> c.txt_enc ppf v
    in
    let txt_dec s ~start = let i, v = c.txt_dec s ~start in i, Some v in
    { c with bin_enc; bin_dec; txt_enc; txt_dec }

  let result ?kind:k ?docvar:d ok error =
    let kind = match k with
    | Some k -> k | None -> Fmt.str "(%s, %s) result" ok.kind error.kind
    in
    let docvar = match d with Some v -> v | None -> "RESULT" in
    let bin_enc b = function
    | Ok v -> Bin.enc_byte b 0x0; ok.bin_enc b v
    | Error e -> Bin.enc_byte b 0x1; error.bin_enc b e
    in
    let bin_dec s ~start = match Bin.dec_byte ~kind s ~start with
    | start, 0x0 -> let i, v = ok.bin_dec s ~start in i, Ok v
    | start, 0x1 -> let i, e = error.bin_dec s ~start in i, Error e
    | _, byte -> Bin.dec_err_exceed ~kind start byte ~max:0x1
    in
    let txt_enc ppf = function
    | Ok v -> Fmt.pf ppf "@[<1>(ok@ @[%a@])@]" ok.txt_enc v
    | Error e -> Fmt.pf ppf "@[<1>(error@ @[%a@])@]" error.txt_enc e
    in
    let txt_dec s ~start =
      let astart = Txt.dec_ls ~kind s ~start in
      let start, a = Txt.dec_atom ~kind s ~start:astart in
      match a with
      | "ok" ->
          let start, v = ok.txt_dec s ~start in
          Txt.dec_le ~kind s ~start, Ok v
      | "error" ->
          let start, e = error.txt_dec s ~start in
          Txt.dec_le ~kind s ~start, Error e
      | a ->
          Txt.dec_err ~kind astart "%s: expected \"ok\" or \"error\"" a
    in
    v ~kind ~docvar bin_enc bin_dec txt_enc txt_dec

  let list ?kind:k ?docvar:d c =
    let kind = match k with Some k -> k | None -> Fmt.str "%s list" c.kind in
    let docvar = match d with Some v -> v | None -> Fmt.str "%s..." c.docvar in
    let bin_enc = Bin.enc_list c.bin_enc in
    let bin_dec = Bin.dec_list c.bin_dec ~kind in
    let txt_enc = Txt.enc_list c.txt_enc in
    let txt_dec = Txt.dec_list ~kind c.txt_dec in
    v ~kind ~docvar bin_enc bin_dec txt_enc txt_dec

  let array ?kind:k ?docvar:d c =
    let kind = match k with Some k -> k | None -> Fmt.str "%s array" c.kind in
    let docvar = match d with Some v -> v | None -> Fmt.str "%s..." c.docvar in
    let bin_enc b vs =
      Bin.enc_len b (Array.length vs); Array.iter (c.bin_enc b) vs
    in
    let bin_dec s ~start =
      let start, len = Bin.dec_len ~kind s ~start in
      match len = 0 with
      | true -> start, [||]
      | false ->
          let start, v0 = c.bin_dec s ~start in
          let a = Array.make len v0 in
          let rec loop a i start = match i < len with
          | false -> start, a
          | true ->
              let start, v = c.bin_dec s ~start in
              a.(i) <- v; loop a (i + 1) start
          in
          loop a 1 start
    in
    let txt_enc ppf vs =
      Fmt.pf ppf "@[<1>(%a)@]" Fmt.(array ~sep:sp c.txt_enc) vs
    in
    let txt_dec s ~start =
      let i, vs = Txt.dec_list ~kind c.txt_dec s ~start in
      i, Array.of_list vs
    in
    v ~kind ~docvar bin_enc bin_dec txt_enc txt_dec

  let pair ?kind:k ?docvar:d c0 c1 =
    let kind = match k with
    | Some k -> k | None -> Fmt.str "%s * %s" c0.kind c1.kind
    in
    let docvar = match d with
    | Some v -> v | None -> Fmt.str "%s %s" c0.docvar c1.docvar
    in
    let bin_enc b (v0, v1) = c0.bin_enc b v0; c1.bin_enc b v1 in
    let bin_dec s ~start =
      let start, v0 = c0.bin_dec s ~start in
      let i, v1 = c1.bin_dec s ~start in
      i, (v0, v1)
    in
    let txt_enc ppf (v0, v1) =
      Fmt.pf ppf "@[<1>(%a %a)@]" c0.txt_enc v0 c1.txt_enc v1
    in
    let txt_dec s ~start =
      let start = Txt.dec_ls ~kind s ~start in
      let start, v0 = c0.txt_dec s start in
      let start, v1 = c1.txt_dec s start in
      let i = Txt.dec_le ~kind s ~start in
      i, (v0, v1)
    in
    v ~kind ~docvar bin_enc bin_dec txt_enc txt_dec

  let enum ~kind ~docvar ?(eq = ( = )) vs =
    let bin_enc b v =
      let rec loop v i = function
      | (_, v') :: _ when eq v v' -> Bin.enc_byte b i
      | _ :: vs -> loop v (i + 1) vs
      | [] -> Bin.enc_err ~kind "unknown enum value"
      in
      loop v 0 vs
    in
    let bin_dec s ~start =
      Bin.dec_need ~kind s ~start ~len:1;
      let rec loop k i = function
      | (_, v) :: _ when k = i -> start + 1, v
      | _ :: vs -> loop k (i + 1) vs
      | [] -> Bin.dec_err ~kind start "%d: unknown enum value index" k
      in
      loop (Char.code s.[start]) 0 vs
    in
    let txt_enc ppf v =
      let rec loop v = function
      | (key, v') :: _  when eq v v' -> Txt.enc_atom ppf key
      | _ :: vs -> loop v vs
      | [] -> Txt.enc_err ~kind "unkown enum value"
      in
      loop v vs
    in
    let txt_dec s ~start =
      let i, key = Txt.dec_atom ~kind s ~start in
      let rec loop key = function
      | (key', v) :: _ when String.equal key key' -> i, v
      | _ :: vs -> loop key vs
      | [] ->
          let ks = List.map fst vs in
          match String.suggest ks key with
          | [] ->
              Txt.dec_err ~kind start
                "%s: unknown enum value, expected one of %a" key
                Fmt.(list ~sep:comma string) ks
          | [k] ->
              Txt.dec_err ~kind start
                "%s: unknown enum value, did you mean %s ?" key k
          | ks ->
              Txt.dec_err ~kind start
                "%s: unknown enum value, did you mean one of %a ?" key
                Fmt.(list ~sep:comma string) ks
      in
      loop key vs
    in
    v ~kind ~docvar bin_enc bin_dec txt_enc txt_dec

  (* Non-composable predefined converters *)

  let string_only =
    let kind = "string" in
    let bin_enc = Bin.enc_bytes in
    let bin_dec = Bin.dec_bytes ~kind in
    let txt_enc = Fmt.string in
    let txt_dec s ~start = String.length s, s in
    v ~kind ~docvar:"VALUE" bin_enc bin_dec txt_enc txt_dec

  let fpath_only =
    let kind = "path" in
    let bin_enc b v = Bin.enc_bytes b (Fpath.to_string v) in
    let bin_dec s ~start =
      let i, v = Bin.dec_bytes ~kind s ~start in
      match Fpath.of_string v with
      | Error e -> Bin.dec_err ~kind start "%s" e
      | Ok v -> i, v
    in
    let txt_enc ppf v = Fmt.string ppf (Fpath.to_string v) in
    let txt_dec s ~start = match Fpath.of_string s with
    | Error e -> Txt.dec_err ~kind start "%s" e
    | Ok v -> (String.length s), v
    in
    v ~kind ~docvar:"PATH" bin_enc bin_dec txt_enc txt_dec
end

module Rqueue = struct
  type 'a t =
    { rand : Random.State.t;
      mutable length : int;
      mutable slots : 'a option array }

  let grow q =
    let slots' = Array.make (2 * q.length) None in
    Array.blit q.slots 0 slots' 0 q.length;
    q.slots <- slots'

  let empty ?(rand = Random.State.make_self_init ()) () =
    { rand; length = 0; slots = Array.make 256 None }

  let add q v =
    if q.length = Array.length q.slots then grow q;
    q.slots.(q.length) <- Some v;
    q.length <- q.length + 1;
    ()

  let take q = match q.length with
  | 0 -> None
  | _ ->
      let i = Random.State.int q.rand q.length in
      let v = match q.slots.(i) with None -> assert false | Some v -> v in
      q.length <- q.length - 1;
      q.slots.(i) <- q.slots.(q.length);
      q.slots.(q.length) <- None;
      Some v

  let length q = q.length
end

(* Binary encoding *)

module Binc = struct
  type 'a enc = Buffer.t -> 'a -> unit
  type 'a dec = string -> int -> int * 'a

  let err i fmt = Fmt.failwith_notrace ("%d: " ^^ fmt) i
  let err_byte ~kind i b =
    err i "corrupted input, unexpected byte 0x%x for %s" b kind

  let check_next ~kind s i next =
   if next <= String.length s then () else
   err i  "unexpected end of input, expected %d bytes for %s" (next - i) kind

  let get_byte s i = Char.code (String.get s i) [@@ocaml.inline]

  let dec_eoi s i =
    if i = String.length s then () else
    err i "expected end of input (len: %d)" (String.length s)

  let enc_magic b magic = Buffer.add_string b magic
  let dec_magic s i magic =
    let next = i + String.length magic in
    check_next ~kind:magic s i next;
    let magic' = String.with_index_range ~first:i ~last:(next - 1) s in
    if String.equal magic magic' then next else
    err i "magic mismatch: %S but expected %S" magic' magic

  let enc_byte b n =
    Buffer.add_char b (Char.chr (n land 0xFF)) [@@ocaml.inline]

  let dec_byte ~kind s i =
    let next = i + 1 in
    check_next ~kind s i next;
    let b = get_byte s i in
    next, b
  [@@ocaml.inline]

  let enc_unit b () = enc_byte b 0
  let dec_unit s i =
    let kind = "unit" in
    let next, b = dec_byte ~kind s i in
    match b with
    | 0 -> next, ()
    | b -> err_byte ~kind i b

  let enc_bool b bool = enc_byte b (if bool then 1 else 0)
  let dec_bool s i =
    let kind = "bool" in
    let next, b = dec_byte ~kind s i in
    match b with
    | 0 -> next, false
    | 1 -> next, true
    | b -> err_byte ~kind i b

  let enc_int b n =
    let w = enc_byte in
    w b n; w b (n lsr 8); w b (n lsr 16); w b (n lsr 24);
    if Sys.word_size = 32 then (w b 0x00; w b 0x00; w b 0x00; w b 0x00) else
    (w b (n lsr 32); w b (n lsr 40); w b (n lsr 48); w b (n lsr 56))

  let dec_int s i =
    let r = get_byte in
    let next = i + 8 in
    check_next ~kind:"int" s i next;
    let b0 = r s (i    ) and b1 = r s (i + 1)
    and b2 = r s (i + 2) and b3 = r s (i + 3) in
    let n = (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0 in
    if Sys.word_size = 32 then next, n else
    let b4 = r s (i + 4) and b5 = r s (i + 5)
    and b6 = r s (i + 6) and b7 = r s (i + 7) in
    next, (b7 lsl 56) lor (b6 lsl 48) lor (b5 lsl 40) lor (b4 lsl 32) lor n

  let enc_int64 b i =
    (* XXX From 4.08 on use Buffer.add_int64_le *)
    let w = enc_byte in
    let i0 = Int64.to_int i in
    let i1 = Int64.to_int (Int64.shift_right_logical i 16) in
    let i2 = Int64.to_int (Int64.shift_right_logical i 32) in
    let i3 = Int64.to_int (Int64.shift_right_logical i 48) in
    let b0 = i0 and b1 = i0 lsr 8 and b2 = i1 and b3 = i1 lsr 8
    and b4 = i2 and b5 = i2 lsr 8 and b6 = i3 and b7 = i3 lsr 8 in
    w b b0; w b b1; w b b2; w b b3; w b b4; w b b5; w b b6; w b b7

  external swap64 : int64 -> int64 = "%bswap_int64"
  external unsafe_get_int64_ne : string -> int -> int64 = "%caml_string_get64u"

  let unsafe_get_int64_le b i = match Sys.big_endian with
  | true -> swap64 (unsafe_get_int64_ne b i)
  | false -> unsafe_get_int64_ne b i

  let dec_int64 s i =
    let next = i + 8 in
    check_next ~kind:"int64" s i next;
    next, unsafe_get_int64_le s i

  let enc_string b s =
    enc_int b (String.length s);
    Buffer.add_string b s

  let dec_string s i =
    let i, len = dec_int s i in
    let next = i + len in
    check_next ~kind:"string" s i next;
    next, String.sub s i len

  let enc_fpath b p = enc_string b (Fpath.to_string p)
  let dec_fpath s i =
    let next, s = dec_string s i in
    match Fpath.of_string s with
    | Error e -> err i "corrupted file path value: %s" e
    | Ok p -> next, p

  let enc_list el b l =
    let rec loop len acc = function
    | [] -> len, acc | v :: vs -> loop (len + 1) (v :: acc) vs
    in
    let len, rl = loop 0 [] l in
    enc_int b len;
    let rec loop el b = function [] -> () | v :: vs -> el b v; loop el b vs in
    loop el b rl

  let dec_list el s i  =
    let i, count = dec_int s i in
    let rec loop el s i count acc = match count = 0 with
    | true -> i, acc (* enc_list writes the reverse list. *)
    | false ->
        let i, v = el s i in
        loop el s i (count - 1) (v :: acc)
    in
    loop el s i count []

  let enc_option w b = function
  | None -> enc_byte b 0
  | Some v -> enc_byte b 1; w b v

  let dec_option some s i =
    let kind = "option" in
    let next, b = dec_byte ~kind s i in
    match b with
    | 0 -> next, None
    | 1 -> let i, v = some s next in i, Some v
    | b -> err_byte ~kind i b

  let enc_result ~ok ~error b = function
  | Ok v -> enc_byte b 0; ok b v
  | Error e -> enc_byte b 1; error b e

  let dec_result ~ok ~error s i =
    let kind = "result" in
    let next, b = dec_byte ~kind s i in
    match b with
    | 0 -> let i, v = ok s next in i, Ok v
    | 1 -> let i, e = error s next in i, Error e
    | b -> err_byte ~kind i b
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
