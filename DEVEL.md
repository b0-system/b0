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
    ocaml boot/strap --clean    # clean the bootstrap build

The `boot/strap` invocation produces the `boot/run` executable which
has B0's own B0 file linked with the `b0` tool driver. Running
`boot/run` it is equivalent to run `b0` on the root `B0.ml`
file. After this usable `b0` executables and libraries are in the
`_b0` directory. These can be used by having the `B0_BOOTSTRAP` and
`B0_DRIVER_BOOTSTRAP` point to the library directories (see
[`boot/env`](boot/env)).

There are a few ways to use the bootstrap built which 
we describe next.

## By locking the b0 root

The recommended way to use the bootstrap build is by executing the
`b0` build unit via `b0 -- b0`.  If you want to use it outside the
`b0` repository to test on other project the best way is to lock `b0`
on the `b0` repository as the environment of the unit execution of
`b0` unlocks it:

    eval $(b0 lock)      # lock b0 on the b0 repo 
    cd ~/project && b0 -- b0  # Run b0 via b0
    eval $(b0 unlock)

This makes sure you are always running an up-to-date `b0`. 

## By using the bowl action

Another
way is to symlink a b0 repo to `bowl` in the source directory

    mkdir -p /tmp/bowl
    b0 -- b0 gather /path/to/repos/* > /tmp/bowl/B0.ml
    ln -s /tmp/bowl bowl
    b0 -- bowl …

## By changing the shell environmen

Another way is to setup your shell environment (for example if you
need to test scripts that run `b0`. This can be done as follows:

    ./boot/env.bat   # let `b0` invocations be the bootstrap build on Windows
    source boot/env  # let `b0` invocations be the bootstrap build on Unix
    cd ~/project && b0 
    unset -f b0      # stop using bootstrap b0 on Unix
    
The disavantage of this approach is that you may run an outdated `b0`
if you made changes and did not rebuild.


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

* [`src/lowtools`] has a few b0 low-level tools: `b0-cache`, 
  `b0-log`, `b0-hash`, `show-uri`.


[`src/std`]: src/std
[`src/memo`]: src/memo
[`src/file`]: src/file
[`src/kit`]: src/kit
[`src/tool`]: src/tool
[`src/lowtools`]: src/lowtools
