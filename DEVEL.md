# Bootstrap

To bootstrap `b0`, we need to find the paths to the `cmdliner` and
`unix` library directories. These are discovered using a few heuristics 
which can be by-passed by explicitly setting:

    export B0_BOOT_CMDLINER_LIBDIR=/path/to/cmdliner/dir
    export B0_BOOT_UNIX_LIBDIR=/path/to/unix/dir

Now the following OCaml scripts can be invoked (see below for
more explanations):

    ocaml boot/strap            # build boot/run and run it
    ocaml boot/strap --no-exec  # only build boot/run
    ./boot/run                  # run the bootstrap build

    ./boot/env.bat   # let `b0` invocations be the bootstrap build on Windows
    source boot/env  # let `b0` invocations be the bootstrap build on Unix
    unset -f b0      # stop using bootstrap b0 on Unix

    ocaml boot/strap --clean    # clean the bootstrap build

The `boot/strap` invocation produces the `boot/run` executable which
has B0's own B0 file linked with the `b0` tool driver. Running
`boot/run` it is equivalent to run `b0` on the root `B0.ml`
file. After this we have usable `b0` executables and libraries in the
`_b0` directory. We can use these by having the `B0_BOOTSTRAP` and 
`B0_DRIVER_BOOTSTRAP` point to the libraries (see [`boot/env`](boot/env)).

# Source map

* [`src/std`] has the `b0.std` library. This has a few things that
  should be in the stdlib and a few others that shouldn't but are
  useful for `b0` based programs.
  
* [`src/memo`] is the core `b0.memo` build library.

* [`src/file`] has the `b0.file` library which for describing software
  construction and deployments and provides programmatic access to the
  definitions of `B0.ml` files.
  
* [`src/kit`] is the `b0.kit` library, it contains build domain 
  specific logics.
  
* [`src/tool`] is the `b0` tool driver. This implements the `b0` tool.

* [`src/low-tools`] has a few b0 low-level tools: `b0-cache`, 
  `b0-log`, `b0-hash`, `show-uri`.


[`src/std`]: src/std
[`src/memo`]: src/memo
[`src/file`]: src/file
[`src/kit`]: src/kit
[`src/tool`]: src/tool
[`src/lowtools`]: src/lowtools
