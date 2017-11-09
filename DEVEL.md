# Running the built `b0` and `d0` tools

The `b0` and `d0` scripts at the toplevel allow to run them. If you
want them as if they were in your PATH do `source dev-env`. This
aliases `b0` and `d0` to these scripts.

# Source tree organisation

* `src-std` this is basically an enhanced stdlib. Nothing specific to
  `b0` lives here.
* `src-lib` this is the core build library. It also defines a few
  derived concepts like variants, deployments and the structure of
  their directory.
* `src-driver` this is the library that handles gathering descriptions
  found in `B0.{b0,ml}` files, their compilation and execution. The
  structure of the `_b0` directory is defined there.
* `src-b0` this is the source of the `b0` tool.
* `src-d0` this is the source of the `d0` tool. 
* `src-care` has the default descriptions provided and visible by
   default in the `B0.ml` files processed by `{b,d}0`. No core concept
   lives there this could have been distributed as seperate
   libraries. It avoids users having to specify these things in
   `B0.b0` files.
