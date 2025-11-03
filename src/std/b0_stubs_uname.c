/*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include "b0_stubs.h"

/* Portable uname queries */

#if defined(OCAML_B0_DARWIN) || defined(OCAML_B0_POSIX) /* inc. Cygwin */

#include <sys/utsname.h>

CAMLprim value ocaml_b0_uname_machine (value unit)
{
  struct utsname u;
  int rc = uname (&u);
  if (rc < 0)
    return caml_copy_string ("unknown");
  else
    return caml_copy_string (u.machine);
}

CAMLprim value ocaml_b0_uname_sysname (value unit)
{
  struct utsname u;
  int rc = uname (&u);
  if (rc < 0)
    return caml_copy_string ("unknown");
  else
    return caml_copy_string (u.sysname);
}

#elif defined(OCAML_B0_WINDOWS)

#include <windows.h>

CAMLprim value ocaml_b0_uname_machine (value unit)
{
  SYSTEM_INFO si;
  GetNativeSystemInfo(&si);
  switch (si.wProcessorArchitecture) {
  /* See https://learn.microsoft.com/en-us/windows/win32/api/sysinfoapi/\
     ns-sysinfoapi-system_info#members */
  case PROCESSOR_ARCHITECTURE_AMD64:
    return caml_copy_string ("x86_64");
  case PROCESSOR_ARCHITECTURE_ARM:
    return caml_copy_string ("arm32"); /* XXX: not sure about 32 here */
  case PROCESSOR_ARCHITECTURE_ARM64:
    return caml_copy_string ("arm64");
  case PROCESSOR_ARCHITECTURE_IA64:
    return caml_copy_string ("ia64");
  case PROCESSOR_ARCHITECTURE_INTEL:
    return caml_copy_string ("i386");
  default:
    return caml_copy_string ("unknown");
  }
}

CAMLprim value ocaml_b0_uname_sysname (value unit)
{ return caml_copy_string ("windows"); }

#else /* Unknown platform */

CAMLprim value ocaml_b0_uname_machine (value unit)
{ return caml_copy_string ("unknown"); }

CAMLprim value ocaml_b0_uname_sysname (value unit)
{ return caml_copy_string ("unknown"); }

#endif

/* Windows Version */

#if defined(OCAML_B0_WINDOWS) || defined(OCAML_B0_CYGWIN)

/* We are being a bit lazy but it feels silly to repeat the code
   that does that in the OCaml runtime system (see win32.c). */
extern unsigned short caml_win32_major;
extern unsigned short caml_win32_minor;
extern unsigned short caml_win32_build;
extern unsigned short caml_win32_revision;

CAMLprim value ocaml_b0_caml_win32_version (value unit)
{
  value version = caml_alloc_small (4, 0);
  Field (version, 0) = Val_int (caml_win32_major);
  Field (version, 1) = Val_int (caml_win32_minor);
  Field (version, 2) = Val_int (caml_win32_build);
  Field (version, 3) = Val_int (caml_win32_revision);
  return version;
}

#else

CAMLprim value ocaml_b0_caml_win32_version (value unit)
{ caml_invalid_argument ("Unsupported"); }

#endif
