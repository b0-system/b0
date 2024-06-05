/*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

//Provides: ocaml_b0_monotonic_now_ns
//Requires: caml_int64_of_float, caml_int64_mul
//Requires: caml_raise_sys_error
function find_performance_obj () {
  var test = function (o)
  { return (o && o.performance && typeof o.performance.now == "function");};

  if (test (globalThis)) { return globalThis.performance; };
  if (test (globalThis.perf_hooks)){ return globalThis.perf_hooks.performance;};
  if (typeof require == "function") {
    var ph = require ("perf_hooks");
    if (test (ph)) { return ph.performance; }
  }
  var obj = { now: function ()
              { caml_raise_sys_error ("performance.now () is not available");}}
  return obj;
}
var performance_obj = find_performance_obj ();
function ocaml_b0_monotonic_now_ns () {
  /* Conversion of DOMHighResTimeStamp to uint64 nanosecond timestamps.

     The spec https://www.w3.org/TR/hr-time-3 says DOMHighResTimeStamp
     are double milliseconds that *should* be accurate to 5 microseconds.
     We simply assume we have microsecond precision and multiply the
     stamps given by performance.now () by 1e3 to get double microseconds.

     We then use Int64.of_float on these double microseconds to get an
     uint64 in microseconds. This works in practice for the following
     reasons. Let us assume we have the largest integer microsecond
     timestamp representable exactly in double, i.e. 2^53 :

     1) Assuming the zero of performance.now is when the tab is created,
        our 2^53 timestamp only occurs after:

        2^53 / 1_000_000 / (24 * 3600 * 365.25) ≅ 285.4 Julian years

     2) 2^53 < Int64.max_int = 2^63 - 1, so seing the result of
        Int64.of_float as unsigned for this timestamp is correct and in
        the defined domain of the conversion function (the truncated float
        must lie in [Int64.min_int;Int64.max_int] for defined behaviour).

        So the Int64.of_float conversion is unlikely to be problematic and
        we simply bring the resulting uint64 microsecond to an uint64
        nanosecond by multiplying by 1000L, which for 2^53 microseconds
        remains smaller than Int64.max_int, yielding a correct uint64
        nanosecond timestamp for a reasonable time range. */

  var now_us = performance_obj.now () * 1e3;
  var now_ns = caml_int64_mul (caml_int64_of_float (now_us),
                               caml_int64_of_float (1000));
  return now_ns;
}
