B0 — Software construction and deployment kit
-------------------------------------------------------------------------------
%%VERSION%%

WARNING this package is unstable and work in progress, do not depend on it. 

B0 describes software construction and deployments using modular and
customizable definitions written in OCaml.

B0 describes:

* The build environment.
* The software configuration, build and testing.
* Source or binary artefacts deployments.
* Development life-cycle procedures and actions.

B0 is built on top of the simple and powerful B00 build library that
treats build tools as deterministic memoizable functions.

The B00 build model provides arbitrary build abstraction and reliable,
efficient incremental rebuilds. The B00 library can be (and has been)
used on its own to devise domain specific build systems.

B0 is distributed under the ISC license. It depends on [cmdliner][cmdliner].

[cmdliner]: https://erratique.ch/software/cmdliner

## Install

b0 can be installed with `opam`:

    opam install b0

If you don't use `opam`, TODO

## Documentation

The documentation can be consulted [online][doc] or via `odig doc b0`.

[doc]: http://erratique.ch/software/b0/doc
