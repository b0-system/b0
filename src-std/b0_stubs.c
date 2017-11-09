/*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
   --------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <stdint.h>

#define OCAML_B0_RAISE_SYS_ERROR(ERR)                               \
  do { caml_raise_sys_error (caml_copy_string("B0 stubs: " ERR)); } \
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

#endif

/* Darwin */

#if defined(OCAML_B0_DARWIN)

#include <mach/mach_time.h>

static mach_timebase_info_data_t scale = {0};
static void ocaml_b0_clock_init_scale (void)
{
  if (mach_timebase_info (&scale) != KERN_SUCCESS)
    OCAML_B0_RAISE_SYS_ERROR ("mach_timebase_info () failed");

  if (scale.denom == 0)
    OCAML_B0_RAISE_SYS_ERROR ("mach_timebase_info_data.denom is 0");
}

CAMLprim value ocaml_b0_monotonic_now_ns (value unit)
{
  if (scale.denom == 0) { ocaml_b0_clock_init_scale (); }
  uint64_t now = mach_absolute_time ();
  return caml_copy_int64 ((now * scale.numer) / scale.denom);
}

/* POSIX */

#elif defined(OCAML_B0_POSIX)

#include <time.h>

CAMLprim value ocaml_b0_monotonic_now_ns (value unit)
{
  struct timespec now;

  if (clock_gettime (CLOCK_MONOTONIC, &now))
    OCAML_B0_RAISE_SYS_ERROR ("clock_gettime () failed");

  return caml_copy_int64 ((uint64_t)(now.tv_sec) *
                          (uint64_t)1000000000 +
                          (uint64_t)(now.tv_nsec));
}

/* Windows */

#elif defined(OCAML_B0_WINDOWS)

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static double freq;
static void ocaml_b0_clock_init_freq (void)
{
  LARGE_INTEGER f;
  if (!QueryPerformanceFrequence(&f))
    OCAML_B0_RAISE_SYS_ERROR ("QueryPerformanceFrequency () failed");
  freq = (1000000000.0 / f.QuadPart);
}

CAMLprim value ocaml_b0_monotonic_now_ns (value unit)
{
  static LARGE_INTEGER now;
  if (freq == 0) ocaml_b0_clock_init_freq;
  if (!QueryPerformanceCounter(&now))
    OCAML_B0_RAISE_SYS_ERROR ("QueryPerformanceCounter () failed");
  return caml_copy_int64 ((uint64_t)(now.QuadPart * freq));
}

/* Unsupported */

#else

#warning OCaml B0 library: unsupported platform, monotonic timings will be wrong

CAMLprim value ocaml_b0_monotonic_now_ns (value unit)
{
  return caml_copy_int64 ((uint64_t)0);
}

#endif

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
