(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [base64] and [base64url] codecs.

    As defined in {{:https://www.rfc-editor.org/rfc/rfc4648}RFC 4684},
    with or without padding. Decoding errors on
    {{:https://datatracker.ietf.org/doc/html/rfc4648#section-3.5}
    non-canonical encodings} to ensure that no two different encodings
    decode to the same byte sequence (see
    {{:https://eprint.iacr.org/2022/361.pdf}this paper}). *)

(** {1:enc_and_padding Encoding and padding specification} *)

type encoding =
[ `Base64
  (** {{:https://www.rfc-editor.org/rfc/rfc4648#section-4}[base64]} *)
| `Base64url
  (** {{:https://www.rfc-editor.org/rfc/rfc4648#section-5}[base64url]} *) ]
(** The type for encodings. *)

type padding =
[ `Padded (** Padded with ['='] characters. *)
| `Unpadded (** Not padded. *) ]
(** The type for specifing
    {{:https://www.rfc-editor.org/rfc/rfc4648#section-3.2}padding}. *)

(** {1:error Decoding errors} *)

type error =
| Invalid_length of int (** Invalid input length *)
| Invalid_letter of char * int (** Invalid letter at index. *)
| Non_canonical_encoding
  (** {{:https://datatracker.ietf.org/doc/html/rfc4648#section-3.5}
      Non-canonical encoding}. *)
(** The type for decoding errors. *)

val error_message : encoding -> error -> string
(** [error_message enc e] is an error message for error [e] while
    decoding encoding [enc]. *)

(** {1:base64 [base64]} *)

val encode : padding -> string -> string
(** [encode p s] is the
    {{:https://www.rfc-editor.org/rfc/rfc4648#section-4}[base64]}
    encoding of [s] padded according to [p]. *)

val decode' : padding -> string -> (string, error) result
(** [decode' p s] is the
    {{:https://www.rfc-editor.org/rfc/rfc4648#section-4}[base64]}
    decode of [s]. If [p] is:
    {ul
    {- [`Padding] the decode expects a padded encoding. The decode
       errors with {!Invalid_length} if the padding is missing.}
    {- [`Unpadded] the decode expect no padding. The decode errors
       with {!Invalid_letter} if there is a padding.}}
    All decodes error with {!Non_canonical_encoding} if a
    padding letter has a non-zero padding. *)

val decode : padding -> string -> (string, string) result
(** [decode] is like {!decode'} with errors mapped by
    {!error_message}[ `Base64]. *)

(** {1:base64url [base64url]} *)

val encode_base64url : padding -> string -> string
(** [encode_base64url] is like {!encode} but for the
    {{:https://www.rfc-editor.org/rfc/rfc4648#section-5}[base64url]}
    encoding. *)

val decode_base64url' : padding -> string -> (string, error) result
(** [decode_base64url'] is like {!decode} but for the
    {{:https://www.rfc-editor.org/rfc/rfc4648#section-5}[base64url]}
    encoding. *)

val decode_base64url : padding -> string -> (string, string) result
(** [decode_base64url] is like {!decode_base64url'} with errors
    mapped by {!error_message}[ `Base64url]. *)
