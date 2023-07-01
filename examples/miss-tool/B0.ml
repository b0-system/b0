open B0_kit.V000
open Fut.Syntax

let t = B0_memo.Tool.by_name "t-9a6a9fed7211"

let missing b =
  let m = B0_build.memo b in
  let t = B0_memo.tool m t in
  B0_memo.spawn m (t Cmd.empty);
  Fut.return ()

let miss_tool = B0_unit.v "miss-tool" missing
