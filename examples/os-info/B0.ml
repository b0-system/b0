
open B0_kit.V000
open B00_std

let notify_info b k =
  let store = B0_build.store b in
  B00.Store.get store B00_os.name @@ fun n ->
  B00.Store.get store B00_os.version @@ fun v ->
  B00.Store.get store B00_os.distribution @@ fun d ->
  B00.Store.get store B00_os.family @@ fun f ->
  B00.Store.get store B00_os.arch @@ fun a ->
  B00.Store.get store B00_os.arch_normalized @@ fun an ->
  B00.Store.get store B00_os.arch_bits @@ fun bits ->
  let m = B0_build.memo b in
  B00.Memo.notify m `Info "@[<v>%a@,%a@,%a@,%a@,%a@,%a@,%a@]"
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
