/*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
   --------------------------------------------------------------------------*/

#include "b0.h"

#include "MurmurHash3.h"
#define XXH_PRIVATE_API
#include "xxhash.h"

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>

#include <sys/stat.h>
#include <sys/mman.h>

static inline void *_caml_b0_mmap (int fd, size_t *size) {
  void *buffer = NULL;

#if defined(OCAML_B0_DARWIN) || defined(OCAML_B0_POSIX)
  struct stat st;

  if (fstat (fd, &st) == -1)
    uerror ("hash_fd/fstat", Val_unit);
  *size = st.st_size;

  if ((buffer = mmap (NULL, *size, PROT_READ, MAP_SHARED, fd, 0)) == MAP_FAILED)
    uerror ("hash_fd/mmap", Val_unit);

  if (madvise (buffer, *size, MADV_SEQUENTIAL | MADV_WILLNEED) == -1)
    uerror ("hash_fd/madvise", Val_unit);
#else
  /* Do mmap for Windows.
     https://msdn.microsoft.com/en-us/library/windows/desktop/aa366556%28v=vs.85%29.aspx */
#warning OCaml B0 library: unsupported platform, hashing will fail.
#endif /* defined(OCAML_B0_DARWIN) || defined(OCAML_B0_POSIX) */

  return buffer;
}

static inline void _caml_b0_munmap (void *buffer, int size) {
#if defined(OCAML_B0_DARWIN) || defined(OCAML_B0_POSIX)
  if (munmap (buffer, size) == -1)
    uerror ("hash_fd/munmap", Val_unit);
#else
#warning OCaml B0 library: unsupported platform, hashing will fail.
#endif /* defined(OCAML_B0_DARWIN) || defined(OCAML_B0_POSIX) */
}

CAMLprim value caml_b0_murmurhash (value str, value ofs, value len, value seed) {
  CAMLparam4 (str, ofs, len, seed);
  CAMLlocal1 (res);
  res = caml_alloc_string (16);
  MurmurHash3_x64_128 (
    Bp_val(str) + Int_val (ofs), Int_val(len), Int_val(seed), Bp_val(res)
  );
  CAMLreturn (res);
}

CAMLprim value caml_b0_murmurhash_fd (value fdv, value seed) {
  CAMLparam2 (fdv, seed);
  CAMLlocal1 (res);
  size_t size = 0;
  void *buffer = _caml_b0_mmap (Int_val (fdv), &size);
  res = caml_alloc_string (16);
  MurmurHash3_x64_128 (buffer, size, Int_val (seed), Bp_val (res));
  _caml_b0_munmap (buffer, size);
  CAMLreturn (res);
}

CAMLprim value caml_b0_xxhash (value str, value ofs, value len, value seed) {
  CAMLparam4 (str, ofs, len, seed);
  CAMLreturn (caml_copy_int64 (
    XXH64 (Bp_val (str) + Int_val (ofs), Int_val (len), Int64_val (seed))
  ));
}

CAMLprim value caml_b0_xxhash_fd (value fdv, value seed) {
  CAMLparam2 (fdv, seed);
  size_t size = 0;
  void *buffer = _caml_b0_mmap (Int_val (fdv), &size);
  XXH64_hash_t res = XXH64 (buffer, size, Int64_val (seed));
  _caml_b0_munmap (buffer, size);
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
