B0 — Software construction care
-------------------------------------------------------------------------------
%%VERSION%%

WARNING this package is unstable and work in progress, do not depend on it. 

B0 describes and executes software construction workflows using
modular definitions written in OCaml. It provides a fully integrated
and customizable software construction experience from development to
deployment.

B0 describes:

* The build environment.
* The configuration, build and testing.
* Source or binary artefacts deployments.
* Custom workflow actions.

B0 is built on top of a simple and powerful build model that treats
build tools as deterministic memoizable functions.

This model is implemented in the B00 library which provides arbitrary
build abstraction and reliable, efficient incremental rebuilds. The
B00 library can be (and has been) used on its own to devise domain
specific build systems.

B0 is distributed under the ISC license. It depends on
[cmdliner][cmdliner]

[cmdliner]: https://erratique.ch/software/cmdliner

## Install

b0 can be installed with `opam`:

    opam install b0

If you don't use `opam`, TODO

## Documentation

The documentation can be consulted [online][doc] or via `odig doc b0`.

[doc]: http://erratique.ch/software/b0/doc
