(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

(* Docker base command *)

type cmd = { cmd : Cmd.t; }

let cmd d = d.cmd
let get () =
  (* TODO abstract this pattern, see `B0_opam` *)
  let absent = Cmd.v "docker" in
  let docker = OS.Env.get_value "B0_DOCKER" Conv.tool ~absent in
  OS.Cmd.resolve docker >>= fun cmd -> Ok { cmd }


(* Managing images. *)

type image = string

type image_build =
  { context : string option;
    dockerfile : Fpath.t;
    opts : Cmd.t }

let image_build ?(opts = Cmd.empty) ?context dockerfile =
  { context; dockerfile; opts }

let image_create d i b = match b.context with
| None ->
    (* FIXME OS.Cmd.run which comes from topkg needs to be given the API
       of bos we can't stdin for now. *)
    failwith "Contextless builds not supported at the moment FIXME"
| Some c ->
    OS.Cmd.run @@
    Cmd.(d.cmd % "build" %% b.opts % "--force-rm" % "-t" % i % "-f" %
         p b.dockerfile % c)

let image_exists d i =
  OS.Cmd.run_out Cmd.(d.cmd % "images" % "-q" % i)
  >>| fun id -> id <> ""

let image_delete d i = image_exists d i >>= function
| false -> Ok ()
| true -> OS.Cmd.(run ~stdout:out_null Cmd.(d.cmd % "rmi" % i))

(* Managing containers. *)

type container = string

let container_exists d c =
  let c = strf "name=%s" c in
  OS.Cmd.run_out Cmd.(d.cmd % "ps" % "-a" % "-q" % "-f" % c)
  >>| fun id -> id <> ""

let container_create d ?workdir:w ?(binds = []) i c =
  let bind (host, cont) = strf "%a:%a" Fpath.pp host Fpath.pp cont in
  let binds = match binds with
  | [] -> Cmd.empty
  | bs -> Cmd.of_list ~slip:"-v" (List.map bind bs)
  in
  let workdir = match w with
  | None -> Cmd.empty
  | Some w -> Cmd.(v "--workdir" % p w)
  in
  let name = Cmd.(v "--name" % c) in
  OS.Cmd.run Cmd.(d.cmd % "run" % "-t" % "-d" %% name %% binds %% workdir % i)

let container_delete d c = container_exists d c >>= function
| false -> Ok ()
| true -> OS.Cmd.run Cmd.(d.cmd % "rm" % "-f" % c)

let container_exec d c cmd =
  OS.Cmd.run_status Cmd.(d.cmd % "exec" % "-it" % c %% cmd)

(* Variants *)

(* FIXME the current binding scheme is not good it doesn't
   allow to use link(2), see TODO.md *)

let run d i c scheme conf =
  let root_dir = Variant.Scheme.proxy_conf_root_dir conf in
  let variant_dir = Variant.Scheme.proxy_conf_variant_dir conf in
  let proxied_scheme = Variant.Scheme.name scheme in
  let bind_root = root_dir, root_dir in
  let bind_cont_b0 =
    (* This binding is important if the host _b0 is in root_dir it
       prevents having its content in the container since we map
       it to somethings else. *)
    Fpath.(variant_dir / "proxy_b0"), Fpath.(root_dir / "_b0")
  in
  let bind_variant =
    (* FIXME variant proxy dir should not be hardcoded *)
    Fpath.(variant_dir / "proxy"),
    (* FIXME at least how to get variant dir should not be hardcoded *)
    Fpath.(root_dir / "_b0" / "v" / proxied_scheme)
  in
  let binds = [bind_root; bind_cont_b0; bind_variant] in
  let workdir = root_dir in
  container_create d ~workdir ~binds i c

let setup i c scheme conf =
  get ()
  >>= fun d -> container_exists d c
  >>= function
  | true -> container_delete d c >>= fun () -> run d i c scheme conf
  | false -> run d i c scheme conf

let create image_build i c scheme conf =
  get ()
  >>= fun d -> image_exists d i
  >>= begin function
  | true -> Ok ()
  | false ->
      match image_build with
      | None -> R.error_msgf "No docker image '%s' found." i
      | Some b -> image_create d i b
  end
  >>= fun () -> setup i c scheme conf

let delete c conf = get () >>= fun d -> container_delete d c

let run i c scheme conf cmd =
  get ()
  >>= fun d -> container_exists d c
  >>= begin function
  | true -> Ok ()
  | false -> setup i c scheme conf
  end
  >>= fun () -> container_exec d c cmd

let variant_scheme ?loc ?doc ?image_build i scheme =
  let sname = Variant.Scheme.name scheme in
  let doc = match doc with
  | Some d -> d
  | None ->
      strf "Build in docker container with image '%s' and scheme '%s'" i sname
  in
  let name = strf "docker-%s" sname in
  let c = strf "b0-%s" name in
  let create = create image_build i c scheme in
  let setup = setup i c scheme in
  let delete = delete c in
  let run = run i c scheme in
  let p = Variant.Scheme.proxy ~create ~setup ~delete ~run scheme in
  Variant.Scheme.v ?loc ~doc name (`Proxy p)

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
