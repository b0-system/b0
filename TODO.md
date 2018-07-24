* Allow tool lookup to be an option
* Review what happens in build outcomes on failure.
* Build part cleaning
* Redo build finish reporting and cycle analysis, start from the parts, not
  from the ops !
* Tool version example.
* OCaml version example/scheme.
* Doc vs synopsis, e.g. for variants. Maybe not. Lookup in the API.
* `b0 env`,  `b0 exec-env` 
* Consider reording tool & conf. Also `Conf` became a mess make it more
  `Build`-like.
* Build need a way to know if we are a root unit or not.
* Serialization move away from Marshal, so that proxy build
  data can be read back.
* Find something forwards looking w.r.t relative paths specified
  to units. We don't want to be in the build fun I think.
  src_root which defaults to sub description location ? 
* `Build.await_file` ? Basically `Build.read` but without
  the read.
* Add `unit path [-b]`
* Page `cmd` info, like we page `log`.
* copy_file handle permissions
* Add a flag to allow multiple writes
* Reproducible builds. Should we force by default BUILD_PATH_PREFIX_MAP
  in the environment ?
  https://reproducible-builds.org/specs/build-path-prefix-map/
* https://hub.docker.com/r/ocaml/opam2-staging/tags/
* B0_ocaml, stub changes do not retrigger some compilation.
  Make library archive depend on stubs. May also be the reason
  for spurious failures in examples.
* b0 log [-f] [-n] [-i]  [--failure] [--reads] [--writes] [--file] [--pkg]
* Distingish paths in `Cmd.t` ? 

# B0.Def

* Provide a way to lock definitions so that any def afterwards raises.
  This ensures that description files do not generate definitions
  dynamically (e.g. conf keys, build units, variants schemes).
* For B0 as a library. Provide a way to disable the retaining of
  definitions (Unit.list, Variant.Scheme.list). Related to locking.

# B0.Conf

Discover should be `Build.t` based. This will give us caching/reacting
to the environment for free and will allow to hide the build aim. Add
a special build unit for it: this can give a directory to write things
in aswell.

# B0.Pkg

Should have a unit associated or maybe not it should be easy to sync
on all the units of a package once we have the notion of package in
units. This should be sufficient for `META` file generation and `opam`
file sync.

# `b0 build` improvements

* Implement `--c-$VAR val ` to set configuration values.
* Improve `-u` unit requests, in particular at the API level we need
  `Build.request : Build.t -> Unit.t -> unit`. Also add `--no-deps`,
  `--no-prev`.

* Incremental performance. Already pretty good but could be better:
** Consider not re-hashing the cache elements, keep their hash in the
   build outcome. I.e. trust that noone fiddles with _b0/cache
** Implement a fast check for the whole build on the roots. This is cheating
   but is useful for e.g. `b0 run` to quickly check and report
   the user is running outdated programs. It's also useful for
   projects like `odig` to see if the doc are up-to-date before
   showing them.
** Implement per unit fast checks. This is less cheating, the parts
   that pass the check don't need to be trashed and relinked and
   we avoid computing all the commands.
** Dolan. Make the cache read only.
** Michel. Hash server using modern fs notification APIs. b0
   simply consults the server.
   
# `b0` driver

* Something is wrong with the error strategy lots of boiler plate. Also the
  variant access API is messy.
* `B0.b0` make `b0-version` really mandatory except on empty files.

# B0.Tool

* Provide support for checking minimal version. Or not.

# Proxy variants and _b0

In `B0_docker` The current scheme means that we copy rather than
link(2). This is due to the way we mount stuff and the business of
excluding parts of `_b0`. The whole way of proxying needs revisiting
so that we can simply mount the root dir without excluding `_b0`. This
should not be a big deal, somehow we only need to deal with `_b0/i` so
that the proxy instance can live there (e.g. setup `B0_DRIVER_DIR`
and/or forward with `--d-dir`).

However we also want to keep the current style for e.g. `ssh` proxies
where we don't want to rsync all the local variants.

# Variants

* Do we want a configuration API ? Also should we really namespace
  the variants ? With the current scheme we end up with a lot of redundancy
  on project composition.
* Logging for setup operations. 
*  Add a suggested name to the Variant.Scheme API, naming a variant after the
   variant scheme may not work that well (but that may be due to 1.).
*  Document/think about how a new scheme can be added e.g. to a whole
   opam switch. (Add a new package with the scheme, set environment
   variable `B0_D_LIBS`.
* Proxy results, they should be stored seperatly so that we don't
   try build them. 
* Proxy we might want to access the namespaced name. E.g. for the docker
   image. 
* Metadata on variants ? Could be useful for cross-variant deployements.
* Review direct functions signature, unify with proxy_conf ? 
* need to distinguish b0 runs from build product runs.
* It is unclear whether proxy scheme should hard-code the proxied
  variant. Maybe this could show up and specified in the cli interface
  e.g. via `--proxy`.

# Build context

A topkg-like build context is needed. It's a bit unclear where this
should fit. Ponder the following alternative.

1. A configuration key in B0_care.
2. A value held by `Env` and thus defined by the variant scheme.

In any case this should be exposed in `Build.t` values.

Also maybe another name than build context should be found. Source
context, run runner, goal.

(*

  In general the behaviour of [b0] tries to be as independent from
  the operating context as possible. However some
  {{!Conf}configuration keys} and/or actions are sensitive to the
    what is called the {e build context}. This makes the invocations
  to specify on the command line and in package descriptions
  (e.g. the [build:] field of [opam] files) terser and thus
    generally improves the DRYness of the tool.

  More specifically [b0] distinguishes between the following contexts:

    {ul
    {- [`Distrib] iff [not (vcs s)]. No VCS is present this is a build from
       a distribution. If there are configuration bits they should be setup
       according to the build environment.}
    {- [`Dev] iff [(vcs c) && not (dev_pkg c)]. This is a development build
       invoked manually in a source repository. The repository checkout
       should likely not be touched and configuration bits not be setup.
       This is happening for example if the developer is building the
       software in her working source repository by invoking [b0].}
    {- [`Dev_pkg] iff [(vcs c) && (dev_pkg c)]. This is a package manager
       development build. In this case the repository checkout may need
       to be massaged into a pseudo-distribution for the software to be
       installed. This meas that distribution watermaking and massaging
       should be performed. See {{!distrib}distribution description}
       and the [prepare_on_pin] argument. Besides existing configuration
       bits should be setup according to the build environment.}}
*)

