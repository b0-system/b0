(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_result

type build_aim = [ `Build_os | `Host_os ]
type tool_lookup = B0_fpath.t list -> B0_fpath.t result

let pp_tool_alts ppf alts =
  let tool = B0_fpath.pp in
  match alts with
  | [] -> B0_fmt.nop ppf ()
  | [t] -> tool ppf t
  | t :: ts ->
      B0_fmt.pf ppf "@[%a or %a@]" tool t B0_fmt.(list ~sep:comma tool) ts

let env_tool_lookup ?sep ?(var = "PATH") env =
  (* FIXME cleanup that *)
  let path = match B0_string.Map.find var env with
  | exception Not_found -> "" | path -> path
  in
  match B0_os.Cmd.search_path_dirs ?sep path with
  | Error _ as e -> (fun _ -> e)
  | Ok search ->
      fun tools ->
        let rec loop = function
        | [] ->
            R.error_msgf "%a: not found in %s=%S" pp_tool_alts tools var path
        | t :: ts ->
            match B0_os.Cmd.find_tool ~search B0_cmd.(v (p t)) with
            | Ok (Some t) -> Ok t
            | Ok None -> loop ts
            | Error _ as e -> e
        in
        loop tools

type t =
  { host_env : B0_os.Env.t;
    host_forced_env : B0_os.Env.t;
    host_lookup : tool_lookup;
    build_env : B0_os.Env.t;
    build_forced_env : B0_os.Env.t;
    build_lookup : tool_lookup; }

let memo lookup =
  let memo = Hashtbl.create 91 in
  fun tool -> match Hashtbl.find memo tool with
  | exception Not_found -> let p = lookup tool in Hashtbl.add memo tool p; p
  | p -> p

let v
    ?build_lookup ?build_forced_env ?build_env ?host_lookup ?host_forced_env
    host_env
  =
  let host_forced_env = match host_forced_env with
  | None -> B0_string.Map.empty
  | Some forced -> forced
  in
  let host_lookup = memo @@ match host_lookup with
  | None -> env_tool_lookup host_env | Some lookup -> lookup
  in
  let build_lookup, build_env = match build_lookup, build_env with
  | None, None -> host_lookup, host_env
  | Some lookup, None -> memo lookup, host_env
  | None, Some build_env -> memo @@ env_tool_lookup build_env, build_env
  | Some lookup, Some build_env -> memo lookup, build_env
  in
  let build_forced_env = match build_forced_env with
  | None -> host_forced_env
  | Some forced -> forced
  in
  { host_env; host_forced_env; host_lookup; build_env; build_forced_env;
    build_lookup }

let env t = function `Host_os -> t.host_env | `Build_os -> t.build_env
let forced_env t = function
| `Host_os -> t.host_forced_env
| `Build_os -> t.build_forced_env

let tool t aim tool = match aim with
| `Host_os -> t.host_lookup tool
| `Build_os -> t.build_lookup tool

(* Environment variables *)

let split_forced_env ?(force_prefix = "B0_FORCE_") env =
  let first = B0_string.length force_prefix in
  let add var v (forced, env) =
    match B0_string.is_prefix ~affix:force_prefix var with
    | false -> forced, (B0_string.Map.add var v env)
    | true ->
        let var = B0_string.with_index_range ~first var in
        (B0_string.Map.add var v forced), env
  in
  B0_string.Map.fold add env (B0_string.Map.empty, B0_string.Map.empty)

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
