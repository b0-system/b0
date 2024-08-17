/*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISCe.
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#define OCAML_B0_RAISE_SYS_ERROR(ERR)                               \
  do { caml_raise_sys_error (caml_copy_string("b0 stubs: " ERR)); } \
  while (0)

/* Detect platform */

#if defined(__APPLE__) && defined(__MACH__)
  #define OCAML_B0_DARWIN

#elif defined(__unix__) || defined(__unix)
 #include <unistd.h>
 #if defined(_POSIX_VERSION)
   #define OCAML_B0_POSIX
 #endif

#elif defined (_WIN32)
#define OCAML_B0_WINDOWS
#define WIN32_LEAN_AND_MEAN

#endif
