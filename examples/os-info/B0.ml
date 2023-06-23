open B0_kit.V000
open B0_std

let notify_info b k =
  let store = B0_build.store b in
  B0_memo.Store.get store B0_os.name @@ fun n ->
  B0_memo.Store.get store B0_os.version @@ fun v ->
  B0_memo.Store.get store B0_os.distribution @@ fun d ->
  B0_memo.Store.get store B0_os.family @@ fun f ->
  B0_memo.Store.get store B0_os.arch @@ fun a ->
  B0_memo.Store.get store B0_os.arch_normalized @@ fun an ->
  B0_memo.Store.get store B0_os.arch_bits @@ fun bits ->
  let m = B0_build.memo b in
  B0_memo.Memo.notify m `Info "@[<v>%a@,%a@,%a@,%a@,%a@,%a@,%a@]"
    Fmt.(field "name" id string) n
    Fmt.(field "version" id string) v
    Fmt.(field "distribution" id string) d
    Fmt.(field "family" id string) f
    Fmt.(field "arch" id string) a
    Fmt.(field "arch-normalized" id string) an
    Fmt.(field "arch-bits" id int) bits;
  k ()

let os_info1 = B0_unit.v "os-info1" notify_info
let os_info2 = B0_unit.v "os-info2" notify_info
