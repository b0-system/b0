/*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
   --------------------------------------------------------------------------*/

#include "b0.h"

#include "MurmurHash3.h"
#define XXH_PRIVATE_API
#include "xxhash.h"

#include <caml/unixsupport.h>

/* Portable mmap/munmap */

/* Darwin and POSIX */

#if defined(OCAML_B0_DARWIN) || defined(OCAML_B0_POSIX)

#include <sys/stat.h>
#include <sys/mman.h>

/* N.B. b0_ctx is not used by this implementation */
#define b0_ctx void *

static inline void *_ocaml_b0_mmap (b0_ctx *c, value fdv, size_t *size)
{
  int fd = Int_val (fdv);
  void *b = NULL;

  struct stat st;

  if (fstat (fd, &st) == -1)
    OCAML_B0_RAISE_SYS_ERROR ("fstat () failed");

  *size = st.st_size;

  if ((b = mmap (NULL, *size, PROT_READ, MAP_SHARED, fd, 0)) == MAP_FAILED)
    OCAML_B0_RAISE_SYS_ERROR ("mmap () failed");

  if (madvise (b, *size, MADV_SEQUENTIAL | MADV_WILLNEED) == -1)
    OCAML_B0_RAISE_SYS_ERROR ("madvise () failed");

  return b;
}

static inline void _ocaml_b0_munmap (b0_ctx *c, void *b, size_t size)
{
  if (munmap (b, size) == -1)
    OCAML_B0_RAISE_SYS_ERROR ("munmap () failed");
}

/* Windows */

#elif defined(OCAML_B0_WINDOWS)

#include <windows.h>

#define b0_ctx HANDLE

static inline void *_ocaml_b0_mmap (b0_ctx *c, value fdv, size_t *size)
{
  HANDLE fd = Handle_val (fdv);
  void *b = NULL;
  LARGE_INTEGER fsize = {0};

  if (!GetFileSizeEx (fd, &fsize))
    OCAML_B0_RAISE_SYS_ERROR ("GetFileSizeEx () failed");

  *size = (size_t)fsize.QuadPart;

  if (*size == 0) return NULL; /* Mapping fails on empty files */

  *c = CreateFileMapping (fd, NULL, PAGE_READONLY, 0, 0, NULL);
  if (!*c)
    OCAML_B0_RAISE_SYS_ERROR ("CreateFileMapping () failed");

  b = MapViewOfFile (*c, FILE_MAP_READ, 0, 0, *size);
  if (!b)
  {
    CloseHandle (c);
    OCAML_B0_RAISE_SYS_ERROR ("MapViewOfFile () failed");
  }

  return b;
}

static inline void _ocaml_b0_munmap (b0_ctx c, void *b, size_t size)
{
  if (size == 0) return; /* The file was empty, no mapping was performed */

  if (!UnmapViewOfFile (b))
    OCAML_B0_RAISE_SYS_ERROR ("UnmapViewOfFile () failed");

  if (!CloseHandle (c))
    OCAML_B0_RAISE_SYS_ERROR ("CloseHandle () failed");
}

/* Unsupported */

#else
#warning OCaml B0 library: unsupported platform, hashing will fail.

#define b0_ctx void *

static inline void *_ocaml_b0_mmap (b0_ctx *c, value fdv, size_t *size)
{ return NULL; }

static inline void _ocaml_b0_munmap (b0_ctx c, void *b, size_t size) {}
#endif

/* Platform independent */

CAMLprim value ocaml_b0_murmurhash (value str, value ofs, value len, value seed)
{
  CAMLparam4 (str, ofs, len, seed);
  CAMLlocal1 (res);
  res = caml_alloc_string (16);
  MurmurHash3_x64_128 (Bp_val(str) + Int_val (ofs),
                       Int_val(len), Int_val(seed), Bp_val(res));
  CAMLreturn (res);
}

CAMLprim value ocaml_b0_murmurhash_fd (value fdv, value seed)
{
  CAMLparam2 (fdv, seed);
  CAMLlocal1 (res);
  size_t size = 0;
  b0_ctx ctx;
  void *b = _ocaml_b0_mmap (&ctx, fdv, &size);
  res = caml_alloc_string (16);
  MurmurHash3_x64_128 (b, size, Int_val (seed), Bp_val (res));
  _ocaml_b0_munmap (ctx, b, size);
  CAMLreturn (res);
}

CAMLprim value ocaml_b0_xxhash (value str, value ofs, value len, value seed)
{
  CAMLparam4 (str, ofs, len, seed);
  CAMLreturn (caml_copy_int64 (XXH64 (Bp_val (str) + Int_val (ofs),
                                      Int_val (len), Int64_val (seed))));
}

CAMLprim value ocaml_b0_xxhash_fd (value fdv, value seed)
{
  CAMLparam2 (fdv, seed);
  size_t size = 0;
  b0_ctx ctx;
  void *b = _ocaml_b0_mmap (&ctx, fdv, &size);
  XXH64_hash_t res = XXH64 (b, size, Int64_val (seed));
  _ocaml_b0_munmap (ctx, b, size);
  CAMLreturn (caml_copy_int64 (res));
}

/*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
