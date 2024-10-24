(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing

let test_byte_size () =
  Test.test "Fmt.byte_size" @@ fun () ->
  let size s = Fmt.str "%a" Fmt.byte_size s in
  Test.string (size 0) "0B" ~__POS__;
  Test.string (size 999) "999B" ~__POS__;
  Test.string (size 1000) "1kB" ~__POS__;
  Test.string (size 1001) "1.01kB" ~__POS__;
  Test.string (size 1010) "1.01kB" ~__POS__;
  Test.string (size 1011) "1.02kB" ~__POS__;
  Test.string (size 1020) "1.02kB" ~__POS__;
  Test.string (size 1100) "1.1kB" ~__POS__;
  Test.string (size 1101) "1.11kB" ~__POS__;
  Test.string (size 1109) "1.11kB" ~__POS__;
  Test.string (size 1111) "1.12kB" ~__POS__;
  Test.string (size 1119) "1.12kB" ~__POS__;
  Test.string (size 1120) "1.12kB" ~__POS__;
  Test.string (size 1121) "1.13kB" ~__POS__;
  Test.string (size 9990) "9.99kB" ~__POS__;
  Test.string (size 9991) "10kB" ~__POS__;
  Test.string (size 9999) "10kB" ~__POS__;
  Test.string (size 10_000) "10kB" ~__POS__;
  Test.string (size 10_001) "10.1kB" ~__POS__;
  Test.string (size 10_002) "10.1kB" ~__POS__;
  Test.string (size 10_099) "10.1kB" ~__POS__;
  Test.string (size 10_100) "10.1kB" ~__POS__;
  Test.string (size 10_100) "10.1kB" ~__POS__;
  Test.string (size 10_101) "10.2kB" ~__POS__;
  Test.string (size 10_199) "10.2kB" ~__POS__;
  Test.string (size 10_199) "10.2kB" ~__POS__;
  Test.string (size 10_200) "10.2kB" ~__POS__;
  Test.string (size 10_201) "10.3kB" ~__POS__;
  Test.string (size 99_901) "100kB" ~__POS__;
  Test.string (size 99_999) "100kB" ~__POS__;
  Test.string (size 100_000) "100kB" ~__POS__;
  Test.string (size 100_001) "101kB" ~__POS__;
  Test.string (size 100_999) "101kB" ~__POS__;
  Test.string (size 101_000) "101kB" ~__POS__;
  Test.string (size 101_001) "102kB" ~__POS__;
  Test.string (size 999_000) "999kB" ~__POS__;
  Test.string (size 999_001) "1MB" ~__POS__;
  Test.string (size 999_999) "1MB" ~__POS__;
  Test.string (size 1_000_000) "1MB" ~__POS__;
  Test.string (size 1_000_001) "1.01MB" ~__POS__;
  Test.string (size 1_009_999) "1.01MB" ~__POS__;
  Test.string (size 1_010_000) "1.01MB" ~__POS__;
  Test.string (size 1_010_001) "1.02MB" ~__POS__;
  Test.string (size 1_019_999) "1.02MB" ~__POS__;
  Test.string (size 1_020_000) "1.02MB" ~__POS__;
  Test.string (size 1_020_001) "1.03MB" ~__POS__;
  Test.string (size 1_990_000) "1.99MB" ~__POS__;
  Test.string (size 1_990_001) "2MB" ~__POS__;
  Test.string (size 1_999_999) "2MB" ~__POS__;
  Test.string (size 2_000_000) "2MB" ~__POS__;
  Test.string (size 9_990_000) "9.99MB" ~__POS__;
  Test.string (size 9_990_001) "10MB" ~__POS__;
  Test.string (size 9_990_999) "10MB" ~__POS__;
  Test.string (size 10_000_000) "10MB" ~__POS__;
  Test.string (size 10_000_001) "10.1MB" ~__POS__;
  Test.string (size 10_099_999) "10.1MB" ~__POS__;
  Test.string (size 10_100_000) "10.1MB" ~__POS__;
  Test.string (size 10_900_001) "11MB" ~__POS__;
  Test.string (size 10_999_999) "11MB" ~__POS__;
  Test.string (size 11_000_000) "11MB" ~__POS__;
  Test.string (size 11_000_001) "11.1MB" ~__POS__;
  Test.string (size 99_900_000) "99.9MB" ~__POS__;
  Test.string (size 99_900_001) "100MB" ~__POS__;
  Test.string (size 99_999_999) "100MB" ~__POS__;
  Test.string (size 100_000_000) "100MB" ~__POS__;
  Test.string (size 100_000_001) "101MB" ~__POS__;
  Test.string (size 100_999_999) "101MB" ~__POS__;
  Test.string (size 101_000_000) "101MB" ~__POS__;
  Test.string (size 101_000_000) "101MB" ~__POS__;
  Test.string (size 999_000_000) "999MB" ~__POS__;
  Test.string (size 999_000_001) "1GB" ~__POS__;
  Test.string (size 999_999_999) "1GB" ~__POS__;
  Test.string (size 1_000_000_000) "1GB" ~__POS__;
  Test.string (size 1_000_000_001) "1.01GB" ~__POS__;
  Test.string (size 1_000_000_001) "1.01GB" ~__POS__;
  ()

let test_uint64_ns_span () =
  Test.test "Fmt.uint64_ns_span" @@ fun () ->
  let span s = Fmt.str "%a" Fmt.uint64_ns_span (Int64.of_string s) in
  Test.string (span "0u0") "0ns" ~__POS__;
  Test.string (span "0u999") "999ns" ~__POS__;
  Test.string (span "0u1_000") "1μs" ~__POS__;
  Test.string (span "0u1_001") "1.01μs" ~__POS__;
  Test.string (span "0u1_009") "1.01μs" ~__POS__;
  Test.string (span "0u1_010") "1.01μs" ~__POS__;
  Test.string (span "0u1_011") "1.02μs" ~__POS__;
  Test.string (span "0u1_090") "1.09μs" ~__POS__;
  Test.string (span "0u1_091") "1.1μs" ~__POS__;
  Test.string (span "0u1_100") "1.1μs" ~__POS__;
  Test.string (span "0u1_101") "1.11μs" ~__POS__;
  Test.string (span "0u1_109") "1.11μs" ~__POS__;
  Test.string (span "0u1_110") "1.11μs" ~__POS__;
  Test.string (span "0u1_111") "1.12μs" ~__POS__;
  Test.string (span "0u1_990") "1.99μs" ~__POS__;
  Test.string (span "0u1_991") "2μs" ~__POS__;
  Test.string (span "0u1_999") "2μs" ~__POS__;
  Test.string (span "0u2_000") "2μs" ~__POS__;
  Test.string (span "0u2_001") "2.01μs" ~__POS__;
  Test.string (span "0u9_990") "9.99μs" ~__POS__;
  Test.string (span "0u9_991") "10μs" ~__POS__;
  Test.string (span "0u9_999") "10μs" ~__POS__;
  Test.string (span "0u10_000") "10μs" ~__POS__;
  Test.string (span "0u10_001") "10.1μs" ~__POS__;
  Test.string (span "0u10_099") "10.1μs" ~__POS__;
  Test.string (span "0u10_100") "10.1μs" ~__POS__;
  Test.string (span "0u10_101") "10.2μs" ~__POS__;
  Test.string (span "0u10_900") "10.9μs" ~__POS__;
  Test.string (span "0u10_901") "11μs" ~__POS__;
  Test.string (span "0u10_999") "11μs" ~__POS__;
  Test.string (span "0u11_000") "11μs" ~__POS__;
  Test.string (span "0u11_001") "11.1μs" ~__POS__;
  Test.string (span "0u11_099") "11.1μs" ~__POS__;
  Test.string (span "0u11_100") "11.1μs" ~__POS__;
  Test.string (span "0u11_101") "11.2μs" ~__POS__;
  Test.string (span "0u99_900") "99.9μs" ~__POS__;
  Test.string (span "0u99_901") "100μs" ~__POS__;
  Test.string (span "0u99_999") "100μs" ~__POS__;
  Test.string (span "0u100_000") "100μs" ~__POS__;
  Test.string (span "0u100_001") "101μs" ~__POS__;
  Test.string (span "0u100_999") "101μs" ~__POS__;
  Test.string (span "0u101_000") "101μs" ~__POS__;
  Test.string (span "0u101_001") "102μs" ~__POS__;
  Test.string (span "0u101_999") "102μs" ~__POS__;
  Test.string (span "0u102_000") "102μs" ~__POS__;
  Test.string (span "0u999_000") "999μs" ~__POS__;
  Test.string (span "0u999_001") "1ms" ~__POS__;
  Test.string (span "0u999_001") "1ms" ~__POS__;
  Test.string (span "0u999_999") "1ms" ~__POS__;
  Test.string (span "0u1_000_000") "1ms" ~__POS__;
  Test.string (span "0u1_000_001") "1.01ms" ~__POS__;
  Test.string (span "0u1_009_999") "1.01ms" ~__POS__;
  Test.string (span "0u1_010_000") "1.01ms" ~__POS__;
  Test.string (span "0u1_010_001") "1.02ms" ~__POS__;
  Test.string (span "0u9_990_000") "9.99ms" ~__POS__;
  Test.string (span "0u9_990_001") "10ms" ~__POS__;
  Test.string (span "0u9_999_999") "10ms" ~__POS__;
  Test.string (span "0u10_000_000") "10ms" ~__POS__;
  Test.string (span "0u10_000_001") "10.1ms" ~__POS__;
  Test.string (span "0u10_000_001") "10.1ms" ~__POS__;
  Test.string (span "0u10_099_999") "10.1ms" ~__POS__;
  Test.string (span "0u10_100_000") "10.1ms" ~__POS__;
  Test.string (span "0u10_100_001") "10.2ms" ~__POS__;
  Test.string (span "0u99_900_000") "99.9ms" ~__POS__;
  Test.string (span "0u99_900_001") "100ms" ~__POS__;
  Test.string (span "0u99_999_999") "100ms" ~__POS__;
  Test.string (span "0u100_000_000") "100ms" ~__POS__;
  Test.string (span "0u100_000_001") "101ms" ~__POS__;
  Test.string (span "0u100_999_999") "101ms" ~__POS__;
  Test.string (span "0u101_000_000") "101ms" ~__POS__;
  Test.string (span "0u101_000_001") "102ms" ~__POS__;
  Test.string (span "0u999_000_000") "999ms" ~__POS__;
  Test.string (span "0u999_000_001") "1s" ~__POS__;
  Test.string (span "0u999_999_999") "1s" ~__POS__;
  Test.string (span "0u1_000_000_000") "1s" ~__POS__;
  Test.string (span "0u1_000_000_001") "1.01s" ~__POS__;
  Test.string (span "0u1_009_999_999") "1.01s" ~__POS__;
  Test.string (span "0u1_010_000_000") "1.01s" ~__POS__;
  Test.string (span "0u1_010_000_001") "1.02s" ~__POS__;
  Test.string (span "0u1_990_000_000") "1.99s" ~__POS__;
  Test.string (span "0u1_990_000_001") "2s" ~__POS__;
  Test.string (span "0u1_999_999_999") "2s" ~__POS__;
  Test.string (span "0u2_000_000_000") "2s" ~__POS__;
  Test.string (span "0u2_000_000_001") "2.01s" ~__POS__;
  Test.string (span "0u9_990_000_000") "9.99s" ~__POS__;
  Test.string (span "0u9_999_999_999") "10s" ~__POS__;
  Test.string (span "0u10_000_000_000") "10s" ~__POS__;
  Test.string (span "0u10_000_000_001") "10.1s" ~__POS__;
  Test.string (span "0u10_099_999_999") "10.1s" ~__POS__;
  Test.string (span "0u10_100_000_000") "10.1s" ~__POS__;
  Test.string (span "0u10_100_000_001") "10.2s" ~__POS__;
  Test.string (span "0u59_900_000_000") "59.9s" ~__POS__;
  Test.string (span "0u59_900_000_001") "1min" ~__POS__;
  Test.string (span "0u59_999_999_999") "1min" ~__POS__;
  Test.string (span "0u60_000_000_000") "1min" ~__POS__;
  Test.string (span "0u60_000_000_001") "1min1s" ~__POS__;
  Test.string (span "0u60_999_999_999") "1min1s" ~__POS__;
  Test.string (span "0u61_000_000_000") "1min1s" ~__POS__;
  Test.string (span "0u61_000_000_001") "1min2s" ~__POS__;
  Test.string (span "0u119_000_000_000") "1min59s" ~__POS__;
  Test.string (span "0u119_000_000_001") "2min" ~__POS__;
  Test.string (span "0u119_999_999_999") "2min" ~__POS__;
  Test.string (span "0u120_000_000_000") "2min" ~__POS__;
  Test.string (span "0u120_000_000_001") "2min1s" ~__POS__;
  Test.string (span "0u3599_000_000_000") "59min59s" ~__POS__;
  Test.string (span "0u3599_000_000_001") "1h" ~__POS__;
  Test.string (span "0u3599_999_999_999") "1h" ~__POS__;
  Test.string (span "0u3600_000_000_000") "1h" ~__POS__;
  Test.string (span "0u3600_000_000_001") "1h1min" ~__POS__;
  Test.string (span "0u3659_000_000_000") "1h1min" ~__POS__;
  Test.string (span "0u3659_000_000_001") "1h1min" ~__POS__;
  Test.string (span "0u3659_999_999_999") "1h1min" ~__POS__;
  Test.string (span "0u3660_000_000_000") "1h1min" ~__POS__;
  Test.string (span "0u3660_000_000_001") "1h2min" ~__POS__;
  Test.string (span "0u3660_000_000_001") "1h2min" ~__POS__;
  Test.string (span "0u3660_000_000_001") "1h2min" ~__POS__;
  Test.string (span "0u3720_000_000_000") "1h2min" ~__POS__;
  Test.string (span "0u3720_000_000_001") "1h3min" ~__POS__;
  Test.string (span "0u7140_000_000_000") "1h59min" ~__POS__;
  Test.string (span "0u7140_000_000_001") "2h" ~__POS__;
  Test.string (span "0u7199_999_999_999") "2h" ~__POS__;
  Test.string (span "0u7200_000_000_000") "2h" ~__POS__;
  Test.string (span "0u7200_000_000_001") "2h1min" ~__POS__;
  Test.string (span "0u86340_000_000_000") "23h59min" ~__POS__;
  Test.string (span "0u86340_000_000_001") "1d" ~__POS__;
  Test.string (span "0u86400_000_000_000") "1d" ~__POS__;
  Test.string (span "0u86400_000_000_001") "1d1h" ~__POS__;
  Test.string (span "0u89999_999_999_999") "1d1h" ~__POS__;
  Test.string (span "0u90000_000_000_000") "1d1h" ~__POS__;
  Test.string (span "0u90000_000_000_001") "1d2h" ~__POS__;
  Test.string (span "0u169200_000_000_000") "1d23h" ~__POS__;
  Test.string (span "0u169200_000_000_001") "2d" ~__POS__;
  Test.string (span "0u169200_000_000_001") "2d" ~__POS__;
  Test.string (span "0u172799_999_999_999") "2d" ~__POS__;
  Test.string (span "0u172800_000_000_000") "2d" ~__POS__;
  Test.string (span "0u172800_000_000_001") "2d1h" ~__POS__;
  Test.string (span "0u31536000_000_000_000") "365d" ~__POS__;
  Test.string (span "0u31554000_000_000_000") "365d5h" ~__POS__;
  Test.string (
    (* Technically this should round to a year but it does get rendered.
       I don't think it matters, it's not inacurate per se. *)
    span "0u31554000_000_000_001") "365d6h" ~__POS__;
  Test.string (span "0u31557600_000_000_000") "1a" ~__POS__;
  Test.string (span "0u31557600_000_000_001") "1a1d" ~__POS__;
  Test.string (span "0u63028800_000_000_000") "1a365d" ~__POS__;
  Test.string (span "0u63093600_000_000_000") "1a365d" ~__POS__;
  Test.string (span "0u63093600_000_000_001") "2a" ~__POS__;
  Test.string (span "0u63115200_000_000_000") "2a" ~__POS__;
  Test.string (span "0u63115200_000_000_001") "2a1d" ~__POS__;
  ()

let test_styling () =
  Test.test "Fmt styling" @@ fun () ->
  (* Warning: in case this gets confusing remember that Test also uses Fmt… *)
  let style = Fmt.styler () in
  let finally () = Fmt.set_styler style in
  Fun.protect ~finally @@ fun () ->
  let b = Buffer.create 255 in
  let ppf = Format.formatter_of_buffer b in
  let r fmt =
    let get () = let s = Buffer.contents b in Buffer.reset b; s in
    Fmt.kpf (fun ppf -> Fmt.flush ppf (); get ()) ppf fmt
  in
  let no_42 () = r "%a No %a" Fmt.puterr () Fmt.(code' int) 42 in
  Fmt.set_styler Fmt.Ansi;
  Test.string (no_42 ()) "\x1B[01;31mError\x1B[m: No \x1b[01m42\x1b[m" ~__POS__;
  Fmt.set_styler Fmt.Plain;
  Test.string (no_42 ()) "Error: No 42" ~__POS__;
  Fmt.set_styler Fmt.Ansi;
  Test.string (no_42 ()) "\x1B[01;31mError\x1B[m: No \x1b[01m42\x1b[m" ~__POS__;
  Fmt.strip_styles ppf; (* Now disable this at the formatter level. *)
  Test.string (no_42 ()) "Error: No 42" ~__POS__;
  ()

let test_text_uchar () =
  Test.test "Fmt.text_uchar" @@ fun () ->
  let u u = Uchar.of_int u in
  Test.string (Fmt.str "%a" Fmt.text_uchar (u 0x0000)) "U+0000" ~__POS__;
  Test.string (Fmt.str "%a" Fmt.text_uchar (u 0x1F42B)) "🐫" ~__POS__;
  ()

let test_text_string_literal () =
  Test.test "Fmt.text_string[_literal]" @@ fun () ->
  Test.string (Fmt.str "%a" Fmt.text_string_literal "\n🐫\"\t")
    "\"\\n🐫\\\"\\u{0009}\"" ~__POS__;
    ()

let main () =
  Test.main @@ fun () ->
  test_byte_size ();
  test_uint64_ns_span ();
  test_styling ();
  test_text_uchar ();
  test_text_string_literal ();
  ()

let () = if !Sys.interactive then () else exit (main ())
