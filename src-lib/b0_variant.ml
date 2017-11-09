(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_result

module Scheme = struct
  type trigger = [ `Before | `After ]

  type kind = [ `Direct of direct | `Proxy of proxy ]

  and direct =
    { preset : B0_conf.Preset.t option;
      setup : B0_conf.t -> unit result;
      delete : B0_conf.t -> unit result;
      env : unit -> B0_env.t result;
      build : trigger -> B0_build.t -> unit result;
      stage : trigger -> B0_outcome.t -> unit result; }

  and proxy_conf =
    { root_dir : B0_fpath.t;
      b0_dir : B0_fpath.t;
      variant_dir : B0_fpath.t; }

  and proxy =
    { proxy_create : proxy_conf -> unit result;
      proxy_setup : proxy_conf -> unit result;
      proxy_delete : proxy_conf -> unit result;
      proxy_run : proxy_conf -> B0_cmd.t -> B0_os.Cmd.status result;
      proxy_scheme : scheme }

  and scheme =
    { def : B0_def.t;
      kind : kind }

  module Scheme = struct
    type t = scheme
    let def_kind = "variant scheme"
    let def_get s = s.def
    let def_namespaced = true
    let def_name_tty_color = `Blue

    let pp_opt_preset =
      B0_fmt.(field "preset" (option ~none:none_stub B0_conf.Preset.pp_name))

    let pp_proxy_scheme =
      B0_fmt.(field "proxy-scheme" string)

    let def_pp_info ppf v =
      B0_fmt.cut ppf ();
      match v.kind with
      | `Direct d -> pp_opt_preset ppf d.preset
      | `Proxy p -> pp_proxy_scheme ppf (B0_def.name p.proxy_scheme.def)
  end

  include B0_def.Make (Scheme)

  (* Direct schemes *)

  let patch_win32_env = match Sys.win32 with
  | false -> fun e -> e
  | true ->
      fun e -> match B0_string.Map.mem "PATH" e with
      | true -> e
      | false ->
          (* For some reason Unix.environment returns the path in this var.
             In fact on Windows environment variables are case insensitive. *)
          match B0_string.Map.find "Path" e with
          | exception Not_found -> e
          | v -> e |> B0_string.Map.remove "Path" |> B0_string.Map.add "PATH" v

  let default_env () = (* build program process environment *)
    B0_os.Env.current () >>= fun env ->
    let env = patch_win32_env env in
    let host_forced_env, host_env = B0_env.split_forced_env env in
    Ok (B0_env.v ~host_forced_env host_env)

  let nop _ = Ok ()
  let nop2 _ _ = Ok ()

  let direct
      ?preset ?(setup = nop) ?(delete = nop) ?env ?(build = nop2)
      ?(stage = nop2) ()
    =
    let env = match env with None -> default_env | Some env -> env in
    { preset; env; setup; build; stage; delete; }

  let direct_preset d = d.preset
  let direct_env d = d.env

  (* Proxy schemes *)

  let proxy_conf ~root_dir ~b0_dir ~variant_dir () =
    { root_dir; b0_dir; variant_dir }

  let proxy_conf_root_dir p = p.root_dir
  let proxy_conf_b0_dir p = p.b0_dir
  let proxy_conf_variant_dir p = p.variant_dir

  let proxy ?(create = nop) ?(setup = nop) ?(delete = nop) ~run scheme =
    { proxy_create = create; proxy_setup = setup; proxy_delete = delete;
      proxy_run = run; proxy_scheme = scheme }

  let proxy_run p = p.proxy_run
  let proxy_create p = p.proxy_create
  let proxy_setup p = p.proxy_create
  let proxy_delete p = p.proxy_delete

  (* Schemes *)

  let v ?loc ?doc n kind =
    let def = def ?loc ?doc n in
    let s = { def; kind } in
    def_add s; s

  let kind s = s.kind

  let nop =
    let doc = "Build in the current execution environment." in
     v "nop" ~doc ~loc:B0_def.Loc.b0 (`Direct (direct ()))

  let with_preset ?loc ?doc s preset name = match s.kind with
  | `Proxy _ -> invalid_arg "Can't set a preset on a proxy scheme"
  | `Direct direct -> v ?loc ?doc name (`Direct { direct with preset })

  let wrap s0 s1 = failwith "Scheme wrapping is TODO"
end

let is_variant_id n = B0_fpath.(is_seg n && not (is_rel_seg n))
let variant_path d n = match is_variant_id n with
| false -> R.error_msgf "Illegal variant name: %S" n
| true -> Ok B0_fpath.(d / n)

let _conf_path path = B0_fpath.(path / "conf")
let _scheme_path path = B0_fpath.(path / "scheme")

type t =
  { name : string;
    path : B0_fpath.t;
    scheme : Scheme.t; }

let scheme v = v.scheme
let conf_path vr = _conf_path vr.path
let outcome_path vr = B0_fpath.(vr.path / "outcome")
let build_path vr = B0_fpath.(vr.path / "b")
let cache_index_path vr = B0_fpath.(vr.path / "index")
let path v = v.path

(* TODO abstract that into Dir_def *)

let value_kind = "variant"
let name v = v.name
let equal v0 v1 = B0_fpath.equal v0.path v1.path
let compare v0 v1 = B0_fpath.compare v0.path v1.path
let compare_by_name v0 v1 = B0_string.compare v0.name v1.name

let pp_name_str ppf s = B0_fmt.tty_str [`Fg `Cyan] ppf s
let pp_name ppf v = pp_name_str ppf (name v)

let pp_synopsis ppf v =
  let bold pp_v ppf v = B0_fmt.tty [`Bold] pp_v ppf v in
  let scheme ppf v = Scheme.pp_name ppf (scheme v) in
  B0_fmt.pf ppf "%a [%a]" (bold pp_name) v scheme v

let pp_info_ext ext =
  let info ppf v =
    let scheme ppf v = (B0_fmt.field "scheme" Scheme.pp_name) ppf (scheme v) in
    let path ppf v = B0_fmt.field "path" B0_fpath.pp ppf (path v)in
    B0_fmt.pf ppf "%a%a@,%a" scheme v ext v path v
  in
  B0_fmt.info ~name:pp_name info

let pp_info = pp_info_ext B0_fmt.nop

type unknown_scheme = [`Unknown_scheme of string * t]
let of_unknown_scheme (`Unknown_scheme (_, v)) = v
let pp_unknown_scheme ppf (`Unknown_scheme (u, v)) =
  B0_fmt.pf ppf "Variant %a: unknown scheme %a." pp_name v Scheme.pp_name_str u

type load = (t, unknown_scheme) Pervasives.result
let of_load = function Ok v -> v | Error (`Unknown_scheme (_, v)) -> v

let exists ~dir name =
  variant_path dir name
  >>= fun path -> B0_os.File.exists (_scheme_path path)

let find ~dir name =
  variant_path dir name
  >>= fun path -> B0_os.Dir.exists path
  >>= function
  | false -> Ok None
  | true ->
      let scheme_path = _scheme_path path in
      B0_os.File.exists scheme_path
      >>= function
      | false -> Ok None
      | true ->
          B0_os.File.read scheme_path >>= fun scheme ->
          match Scheme.find scheme with
          | Some scheme ->  Ok (Some (Ok { name; path; scheme }))
          | None ->
              let v = { name; path; scheme = Scheme.nop} in
              Ok (Some (Error (`Unknown_scheme (scheme, v))))

let get ~dir name = find ~dir name >>= function
| None -> invalid_arg (B0_string.strf "Variant %a unknown" pp_name_str name)
| Some v -> Ok v

let get_or_suggest ~dir name =
  let reword_err _ =
    R.msgf "Could not get or suggest a variant for %a" pp_name_str name
  in
  begin match find ~dir name with
  | Error _ as e -> e
  | Ok Some v -> Ok (Ok v)
  | Ok None ->
      B0_os.Dir.exists dir >>= function
      | false -> Ok (Error [])
      | true ->
          match B0_os.Dir.dirs ~rel:true dir with
          | Error _ as e -> e
          | Ok dirs ->
              let suggs = List.rev_map B0_fpath.to_string dirs in
              Ok (Error (B0_string.suggest suggs name))
  end
  |> R.reword_error_msg reword_err

let list ~dir =
  let reword_err _ = R.msgf "Could not list variants in %a" B0_fpath.pp dir in
  B0_os.Dir.exists dir >>= function
  | false -> Ok []
  | true ->
      B0_os.Dir.dirs ~rel:true dir >>= fun dirs ->
      let rec loop vs = function
      | [] -> Ok vs
      | d :: dirs ->
          match find ~dir (B0_fpath.to_string d) with
          | Error _ as e -> e
          | Ok Some v -> loop (v :: vs) dirs
          | Ok None -> loop vs dirs
      in
      (loop [] (List.sort_uniq B0_fpath.compare dirs))
      |> R.reword_error_msg reword_err

let list_empty ~dir =
  let reword_err _ =
    R.msgf "Could not test for variant emptyness in %a" B0_fpath.pp dir
  in
  B0_os.Dir.exists dir >>= function
  | false -> Ok true
  | true ->
      B0_os.Dir.dirs ~rel:true dir >>= fun names ->
      let rec loop = function
      | [] -> Ok true
      | n :: names ->
          match exists ~dir (B0_fpath.to_string n) with
          | Ok true -> Ok false
          | Ok false -> loop names
          | Error _ as e -> e
      in
      (loop names)
      |> R.reword_error_msg reword_err

(* End of Def *)

let init_conf ~preset scheme conf_path = match preset with
| false -> Ok []
| true ->
    match Scheme.kind scheme with
    | `Proxy _ -> Ok []
    | `Direct d ->
        match Scheme.direct_preset d with
        | None -> Ok []
        | Some preset ->
            Scheme.direct_env d () >>= fun env ->
            let c, errs = B0_conf.of_preset env `Host_os preset in
            B0_conf.save c conf_path >>= fun _ (* FIXME surface that up *) ->
            Ok errs

let create ?(preset = true) ~dir name scheme =
  (* N.B. we check for the scheme path existence rather than variant directory.
     The reason is that directory might exist in proxy builds (e.g. docker
     bind mounts creates them). *)
  variant_path dir name
  >>= fun path -> B0_os.File.exists (_scheme_path path)
  >>= function
  | true ->
      R.error_msgf "Cannot create %a, it already exists." pp_name_str name
  | false ->
      B0_os.Dir.create path
      >>= fun _ -> B0_os.File.write (_scheme_path path) (Scheme.name scheme)
      >>= fun () -> init_conf ~preset scheme (_conf_path path)
      >>= fun errs -> Ok ({ name; path; scheme }, errs)

let reset v =
  B0_os.Dir.delete ~contents:true (build_path v) >>= fun () ->
  B0_os.File.delete (conf_path v) >>= fun () ->
  B0_os.File.delete (cache_index_path v) >>= fun () ->
  B0_os.File.delete (outcome_path v)

let delete ?(force = true) v =
  let scheme_deletion_hook ~force v =
    if true then Ok () else Error (`Scheme (`Msg "type"))
  in
  scheme_deletion_hook ~force v
  >>= fun () -> R.open_error_msg (B0_os.Dir.delete ~contents:true v.path)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers

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
