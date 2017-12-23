# Bootstrap

To boostrap B0 needs to be pointed to the  `cmdliner`, `result` and
`unix` libraries. This can be done with or without `opam`.

1. With `opam` (v2 is needed). irst install the prerequisistes and make
   sure the opam environment is setup:
   ```
        opam install --deps-only .
        eval $(opam env)
   ```
2. Without `opam`. Set the `B0_DRIVER_LIBDIR` environment variable to
   a root library directory. This will lookup the `cmdliner`, `result`
   and `ocaml/unix` directories for corresponding libraries.

Now invoke from the root directory of the distribution:

    ocaml b00t.ml cold

This produces a driver instance `_boot/b0` with b0's own `B0.ml`
description. This executable can be run to build the libraries and the
`b0` and `d0` tools in the `_boot_b0` directory by invoking:

    ocaml b00t.ml build [ARG]...

As long as the description `B0.ml` doesn't change this command can be
used. If the description changes restart from cold. The two steps can
be combined by invoking

    ocaml b00t.ml [full]

To reset the `_boot` directory invoke:

    ocaml b00t.ml reset
    
# Running the boostraped `b0` and `d0` tools

The `b0` and `d0` shell scripts at the toplevel allow to run the `b0`
and `d0` tools as built in `_boot_b0`. If you want those to be added
to your `PATH` as `b0` and `d0` do a `source dev-env`.

On Windows run `dev-env.bat`, this aliases `b0` and `d0` to the
binaries built in `_boot_b0`.

# Source tree organisation

* `src-std` this is basically an enhanced stdlib. Nothing specific to
  `b0` lives here. It also has all the C stubs of the project.
* `src-lib` this is the core build library. A few derived concept like
  variants, deployments and the structure of their directory are
  defined there.
* `src-driver` this is the library that gathers descriptions found in
  `B0.{b0,ml}` files, their compilation and execution. The structure
  of the `_b0` directory is defined there.
* `src-b0` this is the source of the `b0` tool.
* `src-d0` this is the source of the `d0` tool. 
* `src-care` has the default descriptions provided and visible by
   default in the `B0.ml` files processed by `{b,d}0`. No core concept
   lives there this could have been distributed as separate
   libraries. It avoids users having to specify these things in
   `B0.b0` files.
