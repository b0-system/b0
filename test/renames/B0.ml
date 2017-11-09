open B0

let g0 = Conf.Group.v "g" ~doc:"Fst def"
let g1 = Conf.Group.v "g" ~doc:"Snd def"

let k0 = Conf.(key "k" Conv.int ~default:(const 0) ~doc:"Fst def" ~group:g0)
let k1 = Conf.(key "k" Conv.int ~default:(const 0) ~doc:"Snd def" ~group:g1)


let p0 =
  let defs = Conf.Preset.[ def k0 (const 1); def k1 (const 2)] in
  Conf.Preset.v "p" defs ~doc:"Fst def"

let p1 =
  let d _ _ k v = R.error_msgf "hey ho" in
  let defs = Conf.Preset.[ def k0 (const 1); def k1 (discover d)] in
  Conf.Preset.v "p" defs ~doc:"Snd def"
