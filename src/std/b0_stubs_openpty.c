/*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include "b0_stubs.h"

/* Portable openpty */

#if defined(OCAML_B0_DARWIN) || defined(OCAML_B0_POSIX) /* inc. Cygwin */

#include <caml/unixsupport.h>

#if defined(OCAML_B0_DARWIN)
#include <util.h>
#else
#include <pty.h>
#endif

CAMLprim value ocaml_b0_openpty (value unit)
{
  int pty, tty;
  int rc = openpty (&pty, &tty, NULL, NULL, NULL);
  if (rc < 0) caml_uerror ("openpty", Nothing);
  value res = caml_alloc_small (2, 0);
  Field (res, 0) = Val_int (pty);
  Field (res, 1) = Val_int (tty);
  return res;
}

#else

#warning OCaml b0 library: Os.Fd.openpty is unsupported on this platform

CAMLprim value ocaml_b0_openpty (value unit)
{ caml_invalid_argument ("Unsupported"); }

#endif
