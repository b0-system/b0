(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing

let test_byte_size () =
  Test.test "Fmt.byte_size" @@ fun () ->
  let size s = Fmt.str "%a" Fmt.byte_size s in
  Test.string ~__POS__ (size 0) "0B";
  Test.string ~__POS__ (size 999) "999B";
  Test.string ~__POS__ (size 1000) "1kB";
  Test.string ~__POS__ (size 1001) "1.01kB";
  Test.string ~__POS__ (size 1010) "1.01kB";
  Test.string ~__POS__ (size 1011) "1.02kB";
  Test.string ~__POS__ (size 1020) "1.02kB";
  Test.string ~__POS__ (size 1100) "1.1kB";
  Test.string ~__POS__ (size 1101) "1.11kB";
  Test.string ~__POS__ (size 1109) "1.11kB";
  Test.string ~__POS__ (size 1111) "1.12kB";
  Test.string ~__POS__ (size 1119) "1.12kB";
  Test.string ~__POS__ (size 1120) "1.12kB";
  Test.string ~__POS__ (size 1121) "1.13kB";
  Test.string ~__POS__ (size 9990) "9.99kB";
  Test.string ~__POS__ (size 9991) "10kB";
  Test.string ~__POS__ (size 9999) "10kB";
  Test.string ~__POS__ (size 10_000) "10kB";
  Test.string ~__POS__ (size 10_001) "10.1kB";
  Test.string ~__POS__ (size 10_002) "10.1kB";
  Test.string ~__POS__ (size 10_099) "10.1kB";
  Test.string ~__POS__ (size 10_100) "10.1kB";
  Test.string ~__POS__ (size 10_100) "10.1kB";
  Test.string ~__POS__ (size 10_101) "10.2kB";
  Test.string ~__POS__ (size 10_199) "10.2kB";
  Test.string ~__POS__ (size 10_199) "10.2kB";
  Test.string ~__POS__ (size 10_200) "10.2kB";
  Test.string ~__POS__ (size 10_201) "10.3kB";
  Test.string ~__POS__ (size 99_901) "100kB";
  Test.string ~__POS__ (size 99_999) "100kB";
  Test.string ~__POS__ (size 100_000) "100kB";
  Test.string ~__POS__ (size 100_001) "101kB";
  Test.string ~__POS__ (size 100_999) "101kB";
  Test.string ~__POS__ (size 101_000) "101kB";
  Test.string ~__POS__ (size 101_001) "102kB";
  Test.string ~__POS__ (size 999_000) "999kB";
  Test.string ~__POS__ (size 999_001) "1MB";
  Test.string ~__POS__ (size 999_999) "1MB";
  Test.string ~__POS__ (size 1_000_000) "1MB";
  Test.string ~__POS__ (size 1_000_001) "1.01MB";
  Test.string ~__POS__ (size 1_009_999) "1.01MB";
  Test.string ~__POS__ (size 1_010_000) "1.01MB";
  Test.string ~__POS__ (size 1_010_001) "1.02MB";
  Test.string ~__POS__ (size 1_019_999) "1.02MB";
  Test.string ~__POS__ (size 1_020_000) "1.02MB";
  Test.string ~__POS__ (size 1_020_001) "1.03MB";
  Test.string ~__POS__ (size 1_990_000) "1.99MB";
  Test.string ~__POS__ (size 1_990_001) "2MB";
  Test.string ~__POS__ (size 1_999_999) "2MB";
  Test.string ~__POS__ (size 2_000_000) "2MB";
  Test.string ~__POS__ (size 9_990_000) "9.99MB";
  Test.string ~__POS__ (size 9_990_001) "10MB";
  Test.string ~__POS__ (size 9_990_999) "10MB";
  Test.string ~__POS__ (size 10_000_000) "10MB";
  Test.string ~__POS__ (size 10_000_001) "10.1MB";
  Test.string ~__POS__ (size 10_099_999) "10.1MB";
  Test.string ~__POS__ (size 10_100_000) "10.1MB";
  Test.string ~__POS__ (size 10_900_001) "11MB";
  Test.string ~__POS__ (size 10_999_999) "11MB";
  Test.string ~__POS__ (size 11_000_000) "11MB";
  Test.string ~__POS__ (size 11_000_001) "11.1MB";
  Test.string ~__POS__ (size 99_900_000) "99.9MB";
  Test.string ~__POS__ (size 99_900_001) "100MB";
  Test.string ~__POS__ (size 99_999_999) "100MB";
  Test.string ~__POS__ (size 100_000_000) "100MB";
  Test.string ~__POS__ (size 100_000_001) "101MB";
  Test.string ~__POS__ (size 100_999_999) "101MB";
  Test.string ~__POS__ (size 101_000_000) "101MB";
  Test.string ~__POS__ (size 101_000_000) "101MB";
  Test.string ~__POS__ (size 999_000_000) "999MB";
  Test.string ~__POS__ (size 999_000_001) "1GB";
  Test.string ~__POS__ (size 999_999_999) "1GB";
  Test.string ~__POS__ (size 1_000_000_000) "1GB";
  Test.string ~__POS__ (size 1_000_000_001) "1.01GB";
  Test.string ~__POS__ (size 1_000_000_001) "1.01GB";
  ()

let test_uint64_ns_span () =
  Test.test "Fmt.uint64_ns_span" @@ fun () ->
  let span s = Fmt.str "%a" Fmt.uint64_ns_span (Int64.of_string s) in
  Test.string ~__POS__ (span "0u0") "0ns";
  Test.string ~__POS__ (span "0u999") "999ns";
  Test.string ~__POS__ (span "0u1_000") "1μs";
  Test.string ~__POS__ (span "0u1_001") "1.01μs";
  Test.string ~__POS__ (span "0u1_009") "1.01μs";
  Test.string ~__POS__ (span "0u1_010") "1.01μs";
  Test.string ~__POS__ (span "0u1_011") "1.02μs";
  Test.string ~__POS__ (span "0u1_090") "1.09μs";
  Test.string ~__POS__ (span "0u1_091") "1.1μs";
  Test.string ~__POS__ (span "0u1_100") "1.1μs";
  Test.string ~__POS__ (span "0u1_101") "1.11μs";
  Test.string ~__POS__ (span "0u1_109") "1.11μs";
  Test.string ~__POS__ (span "0u1_110") "1.11μs";
  Test.string ~__POS__ (span "0u1_111") "1.12μs";
  Test.string ~__POS__ (span "0u1_990") "1.99μs";
  Test.string ~__POS__ (span "0u1_991") "2μs";
  Test.string ~__POS__ (span "0u1_999") "2μs";
  Test.string ~__POS__ (span "0u2_000") "2μs";
  Test.string ~__POS__ (span "0u2_001") "2.01μs";
  Test.string ~__POS__ (span "0u9_990") "9.99μs";
  Test.string ~__POS__ (span "0u9_991") "10μs";
  Test.string ~__POS__ (span "0u9_999") "10μs";
  Test.string ~__POS__ (span "0u10_000") "10μs";
  Test.string ~__POS__ (span "0u10_001") "10.1μs";
  Test.string ~__POS__ (span "0u10_099") "10.1μs";
  Test.string ~__POS__ (span "0u10_100") "10.1μs";
  Test.string ~__POS__ (span "0u10_101") "10.2μs";
  Test.string ~__POS__ (span "0u10_900") "10.9μs";
  Test.string ~__POS__ (span "0u10_901") "11μs";
  Test.string ~__POS__ (span "0u10_999") "11μs";
  Test.string ~__POS__ (span "0u11_000") "11μs";
  Test.string ~__POS__ (span "0u11_001") "11.1μs";
  Test.string ~__POS__ (span "0u11_099") "11.1μs";
  Test.string ~__POS__ (span "0u11_100") "11.1μs";
  Test.string ~__POS__ (span "0u11_101") "11.2μs";
  Test.string ~__POS__ (span "0u99_900") "99.9μs";
  Test.string ~__POS__ (span "0u99_901") "100μs";
  Test.string ~__POS__ (span "0u99_999") "100μs";
  Test.string ~__POS__ (span "0u100_000") "100μs";
  Test.string ~__POS__ (span "0u100_001") "101μs";
  Test.string ~__POS__ (span "0u100_999") "101μs";
  Test.string ~__POS__ (span "0u101_000") "101μs";
  Test.string ~__POS__ (span "0u101_001") "102μs";
  Test.string ~__POS__ (span "0u101_999") "102μs";
  Test.string ~__POS__ (span "0u102_000") "102μs";
  Test.string ~__POS__ (span "0u999_000") "999μs";
  Test.string ~__POS__ (span "0u999_001") "1ms";
  Test.string ~__POS__ (span "0u999_001") "1ms";
  Test.string ~__POS__ (span "0u999_999") "1ms";
  Test.string ~__POS__ (span "0u1_000_000") "1ms";
  Test.string ~__POS__ (span "0u1_000_001") "1.01ms";
  Test.string ~__POS__ (span "0u1_009_999") "1.01ms";
  Test.string ~__POS__ (span "0u1_010_000") "1.01ms";
  Test.string ~__POS__ (span "0u1_010_001") "1.02ms";
  Test.string ~__POS__ (span "0u9_990_000") "9.99ms";
  Test.string ~__POS__ (span "0u9_990_001") "10ms";
  Test.string ~__POS__ (span "0u9_999_999") "10ms";
  Test.string ~__POS__ (span "0u10_000_000") "10ms";
  Test.string ~__POS__ (span "0u10_000_001") "10.1ms";
  Test.string ~__POS__ (span "0u10_000_001") "10.1ms";
  Test.string ~__POS__ (span "0u10_099_999") "10.1ms";
  Test.string ~__POS__ (span "0u10_100_000") "10.1ms";
  Test.string ~__POS__ (span "0u10_100_001") "10.2ms";
  Test.string ~__POS__ (span "0u99_900_000") "99.9ms";
  Test.string ~__POS__ (span "0u99_900_001") "100ms";
  Test.string ~__POS__ (span "0u99_999_999") "100ms";
  Test.string ~__POS__ (span "0u100_000_000") "100ms";
  Test.string ~__POS__ (span "0u100_000_001") "101ms";
  Test.string ~__POS__ (span "0u100_999_999") "101ms";
  Test.string ~__POS__ (span "0u101_000_000") "101ms";
  Test.string ~__POS__ (span "0u101_000_001") "102ms";
  Test.string ~__POS__ (span "0u999_000_000") "999ms";
  Test.string ~__POS__ (span "0u999_000_001") "1s";
  Test.string ~__POS__ (span "0u999_999_999") "1s";
  Test.string ~__POS__ (span "0u1_000_000_000") "1s";
  Test.string ~__POS__ (span "0u1_000_000_001") "1.01s";
  Test.string ~__POS__ (span "0u1_009_999_999") "1.01s";
  Test.string ~__POS__ (span "0u1_010_000_000") "1.01s";
  Test.string ~__POS__ (span "0u1_010_000_001") "1.02s";
  Test.string ~__POS__ (span "0u1_990_000_000") "1.99s";
  Test.string ~__POS__ (span "0u1_990_000_001") "2s";
  Test.string ~__POS__ (span "0u1_999_999_999") "2s";
  Test.string ~__POS__ (span "0u2_000_000_000") "2s";
  Test.string ~__POS__ (span "0u2_000_000_001") "2.01s";
  Test.string ~__POS__ (span "0u9_990_000_000") "9.99s";
  Test.string ~__POS__ (span "0u9_999_999_999") "10s";
  Test.string ~__POS__ (span "0u10_000_000_000") "10s";
  Test.string ~__POS__ (span "0u10_000_000_001") "10.1s";
  Test.string ~__POS__ (span "0u10_099_999_999") "10.1s";
  Test.string ~__POS__ (span "0u10_100_000_000") "10.1s";
  Test.string ~__POS__ (span "0u10_100_000_001") "10.2s";
  Test.string ~__POS__ (span "0u59_900_000_000") "59.9s";
  Test.string ~__POS__ (span "0u59_900_000_001") "1min";
  Test.string ~__POS__ (span "0u59_999_999_999") "1min";
  Test.string ~__POS__ (span "0u60_000_000_000") "1min";
  Test.string ~__POS__ (span "0u60_000_000_001") "1min1s";
  Test.string ~__POS__ (span "0u60_999_999_999") "1min1s";
  Test.string ~__POS__ (span "0u61_000_000_000") "1min1s";
  Test.string ~__POS__ (span "0u61_000_000_001") "1min2s";
  Test.string ~__POS__ (span "0u119_000_000_000") "1min59s";
  Test.string ~__POS__ (span "0u119_000_000_001") "2min";
  Test.string ~__POS__ (span "0u119_999_999_999") "2min";
  Test.string ~__POS__ (span "0u120_000_000_000") "2min";
  Test.string ~__POS__ (span "0u120_000_000_001") "2min1s";
  Test.string ~__POS__ (span "0u3599_000_000_000") "59min59s";
  Test.string ~__POS__ (span "0u3599_000_000_001") "1h";
  Test.string ~__POS__ (span "0u3599_999_999_999") "1h";
  Test.string ~__POS__ (span "0u3600_000_000_000") "1h";
  Test.string ~__POS__ (span "0u3600_000_000_001") "1h1min";
  Test.string ~__POS__ (span "0u3659_000_000_000") "1h1min";
  Test.string ~__POS__ (span "0u3659_000_000_001") "1h1min";
  Test.string ~__POS__ (span "0u3659_999_999_999") "1h1min";
  Test.string ~__POS__ (span "0u3660_000_000_000") "1h1min";
  Test.string ~__POS__ (span "0u3660_000_000_001") "1h2min";
  Test.string ~__POS__ (span "0u3660_000_000_001") "1h2min";
  Test.string ~__POS__ (span "0u3660_000_000_001") "1h2min";
  Test.string ~__POS__ (span "0u3720_000_000_000") "1h2min";
  Test.string ~__POS__ (span "0u3720_000_000_001") "1h3min";
  Test.string ~__POS__ (span "0u7140_000_000_000") "1h59min";
  Test.string ~__POS__ (span "0u7140_000_000_001") "2h";
  Test.string ~__POS__ (span "0u7199_999_999_999") "2h";
  Test.string ~__POS__ (span "0u7200_000_000_000") "2h";
  Test.string ~__POS__ (span "0u7200_000_000_001") "2h1min";
  Test.string ~__POS__ (span "0u86340_000_000_000") "23h59min";
  Test.string ~__POS__ (span "0u86340_000_000_001") "1d";
  Test.string ~__POS__ (span "0u86400_000_000_000") "1d";
  Test.string ~__POS__ (span "0u86400_000_000_001") "1d1h";
  Test.string ~__POS__ (span "0u89999_999_999_999") "1d1h";
  Test.string ~__POS__ (span "0u90000_000_000_000") "1d1h";
  Test.string ~__POS__ (span "0u90000_000_000_001") "1d2h";
  Test.string ~__POS__ (span "0u169200_000_000_000") "1d23h";
  Test.string ~__POS__ (span "0u169200_000_000_001") "2d";
  Test.string ~__POS__ (span "0u169200_000_000_001") "2d";
  Test.string ~__POS__ (span "0u172799_999_999_999") "2d";
  Test.string ~__POS__ (span "0u172800_000_000_000") "2d";
  Test.string ~__POS__ (span "0u172800_000_000_001") "2d1h";
  Test.string ~__POS__ (span "0u31536000_000_000_000") "365d";
  Test.string ~__POS__ (span "0u31554000_000_000_000") "365d5h";
  Test.string ~__POS__ (
    (* Technically this should round to a year but it does get rendered.
       I don't think it matters, it's not inacurate per se. *)
    span "0u31554000_000_000_001") "365d6h";
  Test.string ~__POS__ (span "0u31557600_000_000_000") "1a";
  Test.string ~__POS__ (span "0u31557600_000_000_001") "1a1d";
  Test.string ~__POS__ (span "0u63028800_000_000_000") "1a365d";
  Test.string ~__POS__ (span "0u63093600_000_000_000") "1a365d";
  Test.string ~__POS__ (span "0u63093600_000_000_001") "2a";
  Test.string ~__POS__ (span "0u63115200_000_000_000") "2a";
  Test.string ~__POS__ (span "0u63115200_000_000_001") "2a1d";
  ()

let main () =
  Test.main @@ fun () ->
  test_byte_size ();
  test_uint64_ns_span ();
  ()

let () = if !Sys.interactive then () else exit (main ())
