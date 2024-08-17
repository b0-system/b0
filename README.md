b0 — Software construction and deployment kit
-------------------------------------------------------------------------------

WARNING this package is unstable and work in progress, do not depend on it. 

b0 describes software construction and deployments using modular and
customizable definitions written in OCaml.

At the core of b0 is the `b0.memo` library which provides arbitrary 
build abstraction with reliable, efficient incremental rebuilds.

b0 is distributed under the ISC license. It depends on [cmdliner][cmdliner].

[cmdliner]: https://erratique.ch/software/cmdliner

## Install

b0 can be installed with `opam`:

    opam install b0

If you don't use `opam`, consult [`DEVEL.md`](DEVEL.md) for bootstrap 
instructions.

## Documentation

The documentation can be consulted [online][doc] or via `odig doc b0`.

[doc]: http://erratique.ch/software/b0/doc
