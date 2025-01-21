(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

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
let failwith_line n fmt = kstr failwith ("%d:" ^^ fmt) n
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

  (*
  (* Doesn't work https://github.com/ocaml/ocaml/issues/13371 *)
  let with_newline ~nl pp = fun ppf v ->
    let outf = Format.pp_get_formatter_out_functions ppf () in
    let out_newline () = outf.out_string nl 0 (String.length nl) in
    let outf_with_nl = { outf with out_newline } in
    let finally () = Format.pp_set_formatter_out_functions ppf outf in
    Format.pp_set_formatter_out_functions ppf outf_with_nl;
    Fun.protect ~finally @@ fun () -> pp ppf v
  *)

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

(* Records *)

let record ?(sep = cut) pps = vbox (concat ~sep pps)

(* Stdlib formatters *)

let bool = Format.pp_print_bool
let int = Format.pp_print_int
let int32 ppf i = pf ppf "%ld" i
let uint32 ppf i = pf ppf "%lu" i
let int64 ppf i = pf ppf "%Ld" i
let uint64 ppf i = pf ppf "%Lu" i
let float ppf f = pf ppf "%g" f
let char = Format.pp_print_char
let string = Format.pp_print_string
let binary_string =
  let pp_byte ppf c = pf ppf "%02x" (Char.code c) in
  iter String.iter pp_byte

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

let backtrace =
  vbox @@ (using Printexc.raw_backtrace_to_string) pp_backtrace_str

let exn ppf e = string ppf (Printexc.to_string e)
let exn_backtrace ppf (e, bt) =
  pf ppf "@[<v>Exception: %a@,%a@]"
    exn e pp_backtrace_str (Printexc.raw_backtrace_to_string bt)

let pair ?sep:(pp_sep = cut) pp_fst pp_snd ppf (fst, snd) =
  pp_fst ppf fst; pp_sep ppf (); pp_snd ppf snd

let none ppf () = string ppf "<none>"
let option ?none:(pp_none = nop) pp_v ppf = function
| None -> pp_none ppf ()
| Some v -> pp_v ppf v

let result ~ok ~error ppf = function Ok v -> ok ppf v | Error e -> error ppf e

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
  [| "y"; "z"; "a"; "f"; "p"; "n"; "\xCE\xBC"; "m"; ""; "k"; "M"; "G"; "T";
     "P"; "E"; "Z"; "Y" |]

let rec pp_at_factor ~scale u symb factor ppf s =
  let m = s / factor in
  let n = s mod factor in
  match m with
  | m when m >= 100 -> (* No fractional digit *)
      let m_up = if n > 0 then m + 1 else m in
      if m_up >= 1000 then si_size ~scale u ppf (m_up * factor) else
      pf ppf "%d@<1>%s%s" m_up symb u
  | m when m >= 10 -> (* One fractional digit w.o. trailing 0 *)
      let f_factor = factor / 10 in
      let f_m = n / f_factor in
      let f_n = n mod f_factor in
      let f_m_up = if f_n > 0 then f_m + 1 else f_m in
      begin match f_m_up with
      | 0 -> pf ppf "%d@<1>%s%s" m symb u
      | f when f >= 10 -> si_size ~scale u ppf (m * factor + f * f_factor)
      | f -> pf ppf "%d.%d@<1>%s%s" m f symb u
      end
  | m -> (* Two or zero fractional digits w.o. trailing 0 *)
      let f_factor = factor / 100 in
      let f_m = n / f_factor in
      let f_n = n mod f_factor in
      let f_m_up = if f_n > 0 then f_m + 1 else f_m in
      match f_m_up with
      | 0 -> pf ppf "%d@<1>%s%s" m symb u
      | f when f >= 100 -> si_size ~scale u ppf (m * factor + f * f_factor)
      | f when f mod 10 = 0 -> pf ppf "%d.%d@<1>%s%s" m (f / 10) symb u
      | f -> pf ppf "%d.%02d@<1>%s%s" m f symb u

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

let us_span   =                  1_000L
let ms_span   =              1_000_000L
let sec_span  =          1_000_000_000L
let min_span  =         60_000_000_000L
let hour_span =       3600_000_000_000L
let day_span  =     86_400_000_000_000L
let year_span = 31_557_600_000_000_000L

let rec pp_si_span unit_str unit_str_len si_unit si_higher_unit ppf span =
  let geq x y = Int64.unsigned_compare x y >= 0 in
  let m = Int64.unsigned_div span si_unit in
  let n = Int64.unsigned_rem span si_unit in
  let pp_unit ppf () = Format.pp_print_as ppf unit_str_len unit_str in
  match m with
  | m when geq m 100L -> (* No fractional digit *)
      let m_up = if Int64.equal n 0L then m else Int64.succ m in
      let span' = Int64.mul m_up si_unit in
      if geq span' si_higher_unit then uint64_ns_span ppf span' else
      (pf ppf "%Ld" m_up; pp_unit ppf ())
  | m when geq m 10L -> (* One fractional digit w.o. trailing zero *)
      let f_factor = Int64.unsigned_div si_unit 10L in
      let f_m = Int64.unsigned_div n f_factor in
      let f_n = Int64.unsigned_rem n f_factor in
      let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
      begin match f_m_up with
      | 0L -> pf ppf "%Ld" m; pp_unit ppf ()
      | f when geq f 10L ->
          uint64_ns_span ppf Int64.(add (mul m si_unit) (mul f f_factor))
      | f -> pf ppf "%Ld.%Ld" m f; pp_unit ppf ()
      end
  | m -> (* Two or zero fractional digits w.o. trailing zero *)
      let f_factor = Int64.unsigned_div si_unit 100L in
      let f_m = Int64.unsigned_div n f_factor in
      let f_n = Int64.unsigned_rem n f_factor in
      let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
      match f_m_up with
      | 0L -> pf ppf "%Ld" m; pp_unit ppf ()
      | f when geq f 100L ->
          uint64_ns_span ppf Int64.(add (mul m si_unit) (mul f f_factor))
      | f when Int64.equal (Int64.rem f 10L) 0L ->
          pf ppf "%Ld.%Ld" m (Int64.div f 10L); pp_unit ppf ()
      | f ->
          pf ppf "%Ld.%02Ld" m f; pp_unit ppf ()

and pp_non_si unit_str unit unit_lo_str unit_lo unit_lo_size ppf span =
  let geq x y = Int64.unsigned_compare x y >= 0 in
  let m = Int64.unsigned_div span unit in
  let n = Int64.unsigned_rem span unit in
  if Int64.equal n 0L then pf ppf "%Ld%s" m unit_str else
  let f_m = Int64.unsigned_div n unit_lo in
  let f_n = Int64.unsigned_rem n unit_lo in
  let f_m_up = if Int64.equal f_n 0L then f_m else Int64.succ f_m in
  match f_m_up with
  | f when geq f unit_lo_size ->
      uint64_ns_span ppf Int64.(add (mul m unit) (mul f unit_lo))
  | f ->
      pf ppf "%Ld%s%Ld%s" m unit_str f unit_lo_str

and uint64_ns_span ppf span =
  let geq x y = Int64.unsigned_compare x y >= 0 in
  let lt x y = Int64.unsigned_compare x y = -1 in
  match span with
  | s when lt s us_span -> pf ppf "%Ldns" s
  | s when lt s ms_span -> pp_si_span "\xCE\xBCs" 2 us_span ms_span ppf s
  | s when lt s sec_span -> pp_si_span "ms" 2 ms_span sec_span ppf s
  | s when lt s min_span -> pp_si_span "s" 1 sec_span min_span ppf s
  | s when lt s hour_span -> pp_non_si "min" min_span "s" sec_span 60L ppf s
  | s when lt s day_span -> pp_non_si "h" hour_span "min" min_span 60L ppf s
  | s when lt s year_span -> pp_non_si "d" day_span "h" hour_span 24L ppf s
  | s ->
      let m = Int64.unsigned_div s year_span in
      let n = Int64.unsigned_rem s year_span in
      if Int64.equal n 0L then pf ppf "%Lda" m else
      let f_m = Int64.unsigned_div n day_span in
      let f_n = Int64.unsigned_rem n day_span in
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

let truncated ~max ppf s =
  if String.length s <= max then Format.pp_print_string ppf s else
  (for i = 0 to max - 2 do Format.pp_print_char ppf s.[i] done;
   Format.pp_print_as ppf 1 "…")

let suffix_lines ~suffix pp ppf v =
  let b = Buffer.create 255 in
  let () = box pp (Format.formatter_of_buffer b) v in
  let lines = Buffer.contents b in
  let lines = String.split_on_char '\n' lines in
  let last = List.length lines - 1 in
  let add_newline_esc i l = if i = last then l else l ^ suffix in
  let lines = List.mapi add_newline_esc lines in
  vbox (list string) ppf lines

let pp_escaped_char ppf c = pf ppf "\\x%02x" (Char.code c)
let ascii_char ppf c =
  if B0__char.Ascii.is_print c then char ppf c else pp_escaped_char ppf c

let _ascii_string ~for_literal ppf s =
  let escape_char ~for_literal c =
    (c = '\"' && for_literal) || not (B0__char.Ascii.is_print c)
  in
  let esc ~for_literal ppf c = match Char.code c with
  | 0x22 when for_literal -> char ppf '\\'; char ppf '\"'
  | 0x5C when for_literal -> char ppf '\\'; char ppf '\\'
  | 0x0D when for_literal -> char ppf '\\'; char ppf 'r'
  | 0x0A when for_literal -> char ppf '\\'; char ppf 'n'
  | _ -> pp_escaped_char ppf c
  in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if escape_char ~for_literal c then esc ~for_literal ppf c else
    char ppf c
  done

let ascii_string ppf s = _ascii_string ~for_literal:false ppf s
let ascii_string_literal ppf s =
  char ppf '\"'; _ascii_string ~for_literal:true ppf s; char ppf '\"'

let text_uchar ppf u =
  let esc = match Uchar.to_int u with
  | u when 0x0000 <= u && u <= 0x001F -> true (* C0 control characters *)
  | u when 0x0080 <= u && u <= 0x009F -> true (* C1 control characters *)
  | 0x2028 -> true (* line separator *)
  | 0x2029 -> true (* paragraph separator *)
  | 0x200E -> true (* left-to-right mark *)
  | 0x200F -> true (* right-to-left mark *)
  | u -> false
  in
  if esc then pf ppf "U+%04X" (Uchar.to_int u) else
  let b = Bytes.create (Uchar.utf_8_byte_length u) in
  ignore (Bytes.set_utf_8_uchar b 0 u);
  Format.pp_print_as ppf 1 (Bytes.unsafe_to_string b)

let _text_string ~ansi ~for_literal ppf s =
  let escape_uchar ~for_literal u = match Uchar.to_int u with
  | 0x0022 -> for_literal
  | u when 0x0000 <= u && u <= 0x001F -> true (* C0 control characters *)
  | u when 0x0080 <= u && u <= 0x009F -> true (* C1 control characters *)
  | 0x2028 -> true (* line separator *)
  | 0x2029 -> true (* paragraph separator *)
  | _ -> false
  in
  let escape ~for_literal ppf u = match Uchar.to_int u with
  | 0x0022 when for_literal -> char ppf '\\'; char ppf '\"'
  | 0x005C when for_literal -> string ppf "\\\\"
  | 0x000A when for_literal -> string ppf "\\n"
  | 0x000D when for_literal -> string ppf "\\r"
  | u -> pf ppf "\\u{%04X}" u
  in
  let rec loop ~for_literal ppf i max =
    if i > max then () else
    let dec = String.get_utf_8_uchar s i in
    let u = Uchar.utf_decode_uchar dec in
    let len = Uchar.utf_decode_length dec in
    let next =
      if ansi && Uchar.to_int u = 0x001B && i + 1 < max && s.[i + 1] = '['
      then begin
        let k = ref (i + 2) in
        while (!k <= max && s.[!k] <> 'm') do incr k done;
        let esc = String.sub s i (!k - i + 1) in
        Format.pp_print_as ppf 0 esc;
        !k + 1
      end else begin
        let () =
          if escape_uchar ~for_literal u then escape ~for_literal ppf u else
          if not (Uchar.utf_decode_is_valid dec) then string ppf "\u{FFFD}"
          else for k = 0 to len - 1 do char ppf (s.[i + k]) done
        in
        i + len
      end
    in
    loop ~for_literal ppf next max
  in
  loop ~for_literal ppf 0 (String.length s - 1)

let text_string ppf s = _text_string ~ansi:false ~for_literal:false ppf s
let text_string_literal ppf s =
  char ppf '\"';
  _text_string ~ansi:false ~for_literal:true ppf s;
  char ppf '\"'

let styled_text_string ppf s =
  _text_string ~ansi:true ~for_literal:false ppf s

let styled_text_string_literal ppf s =
  char ppf '\"';
  _text_string ~ansi:true ~for_literal:true ppf s;
  char ppf '\"'

(* HCI fragments *)

let op_enum op ?(empty = nop) pp_v ppf = function
| [] -> empty ppf ()
| [v] -> pp_v ppf v
| vs ->
    let rec loop ppf = function
    | [v0; v1] -> pf ppf "%a@ %s@ %a" pp_v v0 op pp_v v1
    | v :: vs -> pf ppf "%a,@ " pp_v v; loop ppf vs
    | [] -> assert false
    in
    loop ppf vs

let and_enum ?empty pp_v ppf vs = op_enum "and" ?empty pp_v ppf vs
let or_enum ?empty pp_v ppf vs = op_enum "or" ?empty pp_v ppf vs
let did_you_mean pp_v ppf = function
| [] -> () | vs -> pf ppf "Did@ you@ mean %a ?" (or_enum pp_v) vs

let must_be pp_v ppf = function
| [] -> () | vs -> pf ppf "Must be %a." (or_enum pp_v) vs

let unknown ~kind pp_v ppf v = pf ppf "Unknown %a %a." kind () pp_v v
let unknown' ~kind pp_v ~hint ppf (v, hints) = match hints with
| [] -> unknown ~kind pp_v ppf v
| hints -> unknown ~kind pp_v ppf v; sp ppf (); (hint pp_v) ppf hints

(* Text styling *)

type styler = Ansi | Plain

let styler' =
  ref begin match Sys.getenv_opt "TERM" with
  | None when Sys.backend_type = Other "js_of_ocaml" -> Ansi
  | None | Some "dumb" -> Plain
  | _ -> Ansi
  end

let set_styler styler = styler' := styler
let styler () = !styler'

let strip_styles ppf =
  (* Note: this code makes the assumption that out_string is always
       going to be called without splitting escapes. Since we only
       ever output escapes via pp_print_as this should not happen. *)
  let strip out_string s first len =
    let max = first + len - 1 in
    let flush first last =
      if first > last then () else
      out_string s first (last - first + 1)
    in
    let rec skip_esc i =
      if i > max then scan i i else
      let k = i + 1 in if s.[i] = 'm' then scan k k else skip_esc k
    and scan first i =
      if i > max then flush first max else match s.[i] with
      | '\x1B' -> flush first (i - 1); skip_esc (i + 1)
      | _ -> scan first (i + 1)
    in
    scan first first
  in
  let funs = Format.pp_get_formatter_out_functions ppf () in
  let funs = { funs with out_string = strip funs.out_string } in
  Format.pp_set_formatter_out_functions ppf funs

type color =
[ `Default
| `Black   | `Black_bright
| `Red     | `Red_bright
| `Green   | `Green_bright
| `Yellow  | `Yellow_bright
| `Blue    | `Blue_bright
| `Magenta | `Magenta_bright
| `Cyan    | `Cyan_bright
| `White   | `White_bright ]

let rec sgr_base_int_of_color = function
| `Default -> 9
| `Black -> 0   | `Black_bright -> 60 + 0
| `Red -> 1     | `Red_bright -> 60 + 1
| `Green -> 2   | `Green_bright -> 60 + 2
| `Yellow -> 3  | `Yellow_bright -> 60 + 3
| `Blue -> 4    | `Blue_bright -> 60 + 4
| `Magenta -> 5 | `Magenta_bright -> 60 + 5
| `Cyan -> 6    | `Cyan_bright -> 60 + 6
| `White -> 7   | `White_bright -> 60 + 7

let sgr_of_fg_color c = string_of_int (30 + sgr_base_int_of_color c)
let sgr_of_bg_color c = string_of_int (40 + sgr_base_int_of_color c)

type style =
[ `Bold | `Faint | `Italic | `Underline | `Blink of [ `Slow | `Rapid ]
| `Reverse | `Fg of color | `Bg of color ]

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
let ansi_esc = "\x1B["
let sgr_reset = "\x1B[m"

let st' styles pp_v ppf v = match !styler' with
| Plain -> pp_v ppf v
| Ansi ->
    (* This doesn't compose well, we should get the current state
       and restore it afterwards rather than resetting. But then we don't
       have access to the current state. *)
    let sgrs = String.concat "" [ansi_esc; sgrs_of_styles styles; "m"] in
    Format.pp_print_as ppf 0 sgrs;
    pp_v ppf v;
    Format.pp_print_as ppf 0 sgr_reset

let st styles ppf s = match !styler' with
| Plain -> string ppf s
| Ansi ->
    let sgrs = String.concat "" [ansi_esc; sgrs_of_styles styles; "m"] in
    Format.pp_print_as ppf 0 sgrs;
    Format.pp_print_string ppf s;
    Format.pp_print_as ppf 0 sgr_reset

let code' pp_v ppf v = st' [`Bold] pp_v ppf v
let code ppf v = st [`Bold] ppf v
let hey ppf v = st [`Bold; `Fg `Red] ppf v
let puterr ppf () = st [`Bold; `Fg `Red] ppf "Error"; char ppf ':'
let putwarn ppf () = st [`Bold; `Fg `Yellow] ppf "Warning"; char ppf ':'
let putnote ppf () = st [`Bold; `Fg `Blue] ppf "Note"; char ppf ':'

let field ?(label = st [`Fg `Yellow]) ?(sep = any ":@ ") l prj pp_v ppf v =
  pf ppf "@[<1>%a%a%a@]" label l sep () pp_v (prj v)

let field ?(label = st [`Fg `Yellow]) ?(sep = any ":@ ") l prj pp_v ppf v =
  pf ppf "@[<1>%a%a%a@]" label l sep () pp_v (prj v)
