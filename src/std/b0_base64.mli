(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Base64 codec.

    Codecs Base64 according to
    {{:https://tools.ietf.org/html/rfc4648}RFC 4684}. *)

val encode : string -> string
(** [encode s] is the Base64 encoding of [s]. *)

val decode : string -> (string, int) result
(** [decode s] is the Base64 decode of [s]. In case of error the
    integer indicates the byte index of the error for an invalid
    alphabet character error or the length of the string if the string
    length is not a multiple of [4]. *)
