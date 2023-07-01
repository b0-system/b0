open B0_kit.V000
open Fut.Syntax

let notify_info b =
  let store = B0_build.store b in
  let* n = B0_store.get store B0_os.name in
  let* v = B0_store.get store B0_os.version in
  let* d = B0_store.get store B0_os.distribution in
  let* f = B0_store.get store B0_os.family in
  let* a = B0_store.get store B0_os.arch in
  let* an = B0_store.get store B0_os.arch_normalized in
  let* bits = B0_store.get store B0_os.arch_bits in
  let m = B0_build.memo b in
  B0_memo.notify m `Info "@[<v>%a@,%a@,%a@,%a@,%a@,%a@,%a@]"
    Fmt.(field "name" id string) n
    Fmt.(field "version" id string) v
    Fmt.(field "distribution" id string) d
    Fmt.(field "family" id string) f
    Fmt.(field "arch" id string) a
    Fmt.(field "arch-normalized" id string) an
    Fmt.(field "arch-bits" id int) bits;
  Fut.return ()

let os_info1 = B0_unit.v "os-info1" notify_info
let os_info2 = B0_unit.v "os-info2" notify_info
