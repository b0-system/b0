# Bootstrap

To bootstrap `b0`, we need to find the paths to the `cmdliner` and
`unix` library directories. These are discovered using a few heuristics 
which can be by-passed by explicitly setting:

    export B0_BOOT_CMDLINER_LIBDIR=/path/to/cmdliner/dir
    export B0_BOOT_UNIX_LIBDIR=/path/to/unix/dir

Now the following OCaml scripts can be invoked (see below for
more explanations):

    ocaml b00t/strap            # build b00t/run and run it
    ocaml b00t/strap --no-exec  # only build b00t/run
    ./b00t/run                  # run the bootstrap build

    ./b00t/env.bat   # let `b0` invocations be the bootstrap build on Windows
    source b00t/env  # let `b0` invocations be the bootstrap build on Unix
    unset -f b0      # stop using bootstrap b0 on Unix

    ocaml b00t/strap --clean    # clean the bootstrap build

The `b00t/strap` invocation produces the `b00t/run` executable which
has B0's own B0 file linked with the `b0` tool driver. Running
`b00t/run` it is equivalent to run `b0` on the root `B0.ml`
file. After this we have usable `b0` executables and libraries in the
`_b0` directory. We can use these by having the `B0_BOOTSTRAP` and 
`B0_DRIVER_BOOTSTRAP` point to the libraries (see [`b00t/env`](b00t/env)).

# Source map

* [`src/std`](src/std) has the `b0.std` library. This has a few things
  that should be in the stdlib and a few others that shouldn't but are
  useful for `b0` based programs.
* [`src/memo`](src/memo) is the core `b0.memo` build library.
* [`src/file`](src/file) has the `b0.file` library which for describing 
  software construction and deployments and provides programmatic access to 
  the definitions of `B0.ml` files.
* [`src/kit`](src/kit) is the `b0.kit` library. Everything but the 
  the kitchen sink.
* [`src/tool`](src/tool) is b0 tool driver. This implements the `b0` tool.
* [`tools`](tools) has a few b0 low-level tools. 

