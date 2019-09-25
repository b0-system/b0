(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std

let logical_cpu_count ?search () =
  Result.map_error (fun e -> Fmt.str "cpu count determination: %s" e) @@
  let int v = try Ok (int_of_string v) with
  | Failure _ -> Fmt.error "%S: not an integer" v
  in
  let win32 () = Os.Env.find' ~empty_is_none:true int "NUMBER_OF_PROCESSORS" in
  if Sys.win32 then win32 () else
  let try_cmd cmd otherwise = Result.bind (Os.Cmd.find cmd) @@ function
  | None -> otherwise ()
  | Some cmd ->
      Result.bind (Os.Cmd.run_out ~trim:true cmd) @@ fun s ->
      Result.map Option.some (int s)
  in
  try_cmd Cmd.(arg "getconf" % "_NPROCESSORS_ONLN") @@ fun () ->
  try_cmd Cmd.(arg "sysctl" % "-n" % "hw.ncpu") @@ fun () ->
  Ok None

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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
