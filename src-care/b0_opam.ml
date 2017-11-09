(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

(* Low level interface *)

type pkg = string
type cmd = { cmd : Cmd.t; }

let get () =
  let opam = OS.Env.opt_var "B0_OPAM" ~absent:"opam" in
  match OS.Cmd.which_raw opam with
  | None -> R.error_msgf "opam: not found in PATH (as %s)" opam
  | Some opam -> Ok { cmd = Cmd.v opam }

let cmd opam = opam.cmd
let env opam ~switch =
  let parse_env_sexp sexps =
    let parse_binding acc = function
    | `List [`Atom var, _; `Atom v, _], _ -> String.Map.add var v acc
    | _, loc ->
        failwith (strf "%a: expected (key value) s-expression" Sexp.pp_loc loc)
    in
    try match sexps with
    | [] -> R.error_msgf "no s-expression in input"
    | (`List binds, loc) :: _  ->
        Ok (List.fold_left parse_binding String.Map.empty binds)
    | (`Atom _, loc) :: _ ->
        R.error_msgf
          "%a: expected list of (key value) s-expressions" Sexp.pp_loc loc
    with
    | Failure e -> R.error_msg e
  in
  let cmd = Cmd.(opam.cmd % "env" % "--sexp" % "--switch" % switch) in
  OS.Cmd.(run_out cmd |> to_string)
  >>= fun out -> Sexp.of_string ~src:(Sexp.File OS.File.dash) out
  >>= fun sexps -> parse_env_sexp sexps

(* Variants *)

let opam_b0_env ?build_switch host_switch () =
  get ()
  >>= fun opam -> env opam ~switch:host_switch
  >>= fun host_env -> match build_switch with
  | None -> Ok (Env.v host_env)
  | Some build_switch ->
      env opam ~switch:build_switch
      >>= fun build_env -> Ok (Env.v ~build_env host_env)

let variant_scheme ?loc ?doc ?preset ?autodep ?pkgs ?build_switch host_switch =
  let doc = match doc with
  | Some d -> d
  | None ->
      match build_switch with
      | None -> strf "Build with opam switch %s" host_switch
      | Some build_switch ->
          strf "Build with opam switch %s for build and %s for host"
            build_switch host_switch
  in
  let name = match build_switch with
  | None -> strf "opam-%s" host_switch
  | Some build_switch -> strf "opam-%s-%s" build_switch host_switch
  in
  let env = opam_b0_env ?build_switch host_switch in
  let d = Variant.Scheme.direct ?preset ~env () in
  Variant.Scheme.v ?loc ~doc name (`Direct d)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
