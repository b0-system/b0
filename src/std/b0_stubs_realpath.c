/*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISCe.
  ---------------------------------------------------------------------------*/

#include "b0_stubs.h"
#include <caml/unixsupport.h>

/* Portable realpath */

/* Darwin and POSIX */

#if defined(OCAML_B0_DARWIN) || defined(OCAML_B0_POSIX)

#include <stdlib.h>

CAMLprim value ocaml_b0_realpath (value p)
{
  char *r = realpath (String_val (p), NULL);
  if (r == NULL) { uerror ("realpath", p); }
  value rp = caml_copy_string (r);
  free (r);
  return rp;
}

/* Windows */

#elif defined(OCAML_B0_WINDOWS)

#include <stdio.h>
#include <windows.h>
#include <fileapi.h>
#include <stdio.h>

#include <caml/osdeps.h>

CAMLprim value ocaml_b0_realpath (value p)
{
  HANDLE h;
  wchar_t *wp;
  wchar_t *wr;
  DWORD wr_len;
  value rp;

  wp = caml_stat_strdup_to_utf16 (String_val (p));
  h = CreateFileW (wp, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING,
                   FILE_FLAG_BACKUP_SEMANTICS, NULL);
  caml_stat_free (wp);

  if (h == INVALID_HANDLE_VALUE)
  {
    win32_maperr (GetLastError ());
    uerror ("realpath", p);
  }

  wr_len = GetFinalPathNameByHandleW (h, NULL, 0, VOLUME_NAME_DOS);
  if (wr_len == 0)
  {
    win32_maperr (GetLastError ());
    CloseHandle (h);
    uerror ("realpath", p);
  }

  wr = caml_stat_alloc ((wr_len + 1) * sizeof (wchar_t));
  wr_len = GetFinalPathNameByHandleW (h, wr, wr_len, VOLUME_NAME_DOS);

  if (wr_len == 0)
  {
    win32_maperr (GetLastError ());
    CloseHandle (h);
    caml_stat_free (wr);
    uerror ("realpath", p);
  }

  rp = caml_copy_string_of_utf16 (wr);
  CloseHandle (h);
  caml_stat_free (wr);
  return rp;
}

/* Unsupported */

#else
#warning OCaml B0 library: unsupported platform, realpath will fail.

CAMLPrim value ocaml_b0_realpath (value p)
{
  OCAML_B0_RAISE_SYS_ERROR ("realpath unimplemented on this platform")
}
#endif
