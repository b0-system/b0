/*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
   --------------------------------------------------------------------------*/

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

#include <windows.h>
#include <stdio.h>
#include <caml/osdeps.h>

CAMLPrim value ocaml_b0_realpath (value p)
{
  HANDLE h;
  wchar_t *wp;
  wchar_t *wr;
  DWORD wr_len;
  value wp;


  wp = caml_stat_strdup_to_utf16 (String_val (p));
  h = CreateFileW (wp, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING,
                   FILE_ATTRIBUTE_NORMAL, NULL);
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

  wr = caml_stat_alloc ((wr_len + 1) * sizeof (w_char_t));
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

/*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*/
