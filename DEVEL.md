# Bootstrap

To bootstrap `b0`, we need to find the paths to the `cmdliner` and
`unix` library directories. These are discovered using a few heuristics 
which can be by-passed by explicitly setting:

```
export B0_BOOT_CMDLINER_LIBDIR=/path/to/cmdliner/dir
export B0_BOOT_UNIX_LIBDIR=/path/to/unix/dir
```

Now invoke: 

```
ocaml b00t/strap  # produces b00t/run
ocaml b00t/run    # builds b0 via b0 itself
```

The first invocation produces the `b00t/run` executable which has B0's
own B0 file linked with a `b0` driver. Running it is equivalent to run
`b0` on the root `B0.ml` file. The executable must be recompiled
if `B0.ml` changes or if the B0 libraries change in a meaningful way
for it's interpretation.

To clean the bootstrap build issue:

```
ocaml b00t/clean
```

## Adding the boostrapped `b0` and `d0` tools to your environment

To add the built `b0` and `d0` tools do 

```
source b00t/env
./b00t/env.bat # on windows
```

# Source map

There is one OCaml library per directory. We distinguish two levels
`b00` and `b0`.

## B00

This is the core infrastructure that can be used on its own to devise
build tools (e.g. `odig`, `brzo`).

* `src-b00/std` has the `b0.b00.std` library. This has a few things that 
  should be in the stdlib and a few others that shouldn't but are useful 
  for `b00` based programs.
* `src-b00` is the core `b0.b00` build library.
* `src-b00/kit` is the `b0.b00.kit` library. An end-user toolkit for working
   with the `b00` API.
* `tools` has a few b00 low-level tools. 

## B0 

This is the system for describing software contruction and deployments
via B0 files.

* `src` has the `b0` library which for describing software construction 
  and deployments.
* `src/kit` is the `b0.kit` library. An end-user toolkit for B0 files. 
* `src/driver` is the `b0.driver` library. It provides programmatic
  access to the definitions of B0 files.
* `tool-b0`, is the `b0.driver.b0` library. The driver library
  for the `b0` tool.





