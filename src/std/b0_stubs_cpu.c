/*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include "b0_stubs.h"

/* Portable cpu information */

#if defined(OCAML_B0_DARWIN) || defined(OCAML_B0_POSIX)

#include <unistd.h>

CAMLprim value ocaml_b0_cpu_logical_count (value unit)
{
  int n = sysconf (_SC_NPROCESSORS_ONLN);
  if (n < 0) { n = 1; }
  return Val_int (n);
}

#elif defined(OCAML_B0_WINDOWS)

#include <windows.h>

CAMLprim value ocaml_b0_cpu_logical_count (value unit)
{
  SYSTEM_INFO i;
  GetSystemInfo (&i);
  DWORD n = i.dwNumberOfProcessors;
  if (n < 0) { n = 1; }
  return Val_int (n);
}

#else /* Unknown */

#warning OCaml b0 library: unknown platform, cpu count will always be 1

CAMLprim value ocaml_b0_cpu_logical_count (value unit)
{ return Val_int (1); }

#endif
