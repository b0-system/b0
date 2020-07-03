OB# Bootstrap

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

There is one OCaml library per directory. We distinguish two levels:
`b00` and `b0`.

## B00

This is the core infrastructure that can be used on its own to devise
build tools (e.g. `odig`, `brzo`).

* [`src-b00/std`](src-b00/std) has the `b0.b00.std` library. This has
  a few things that should be in the stdlib and a few others that shouldn't 
  but are useful for `b00` based programs.
* [`src-b00`](src-b00) is the core `b0.b00` build library.
* [`src-b00/kit`](src-b00/kit) is the `b0.b00.kit` library. An
   end-user toolkit for working with the `b00` API.
* [`tools`](tools) has a few b00 low-level tools. 

## B0 

This is the system for describing software contruction and deployments
via B0 files.

* [`src`](src) has the `b0` library which for describing software construction 
  and deployments and provides programmatic access to the definitions of B0 files.
* [`src/kit`](src/kit) is the `b0.kit` library. An end-user toolkit
  for B0 files.
* [`tool-b0`](tool-b0) is b0 tool driver.
