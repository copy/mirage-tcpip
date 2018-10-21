(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** One's complement checksum, RFC1071 *)
(* external ones_complement: Cstruct.t -> int = "caml_tcpip_ones_complement_checksum" *)

(* external ones_complement_list: Cstruct.t list -> int = "caml_tcpip_ones_complement_checksum_list" *)

let ones_complement (buffer: Cstruct.t) =
  let len = Cstruct.len buffer in
  let rec finalise_checksum cs =
    assert (cs >= 0);
    if cs < 0x10000 then
      cs
    else
      finalise_checksum ((cs land 0xffff) + (cs lsr 16))
  in
  let rec do_checksum checksum offset =
    if offset + 1 < len then (
      let checksum = checksum + Cstruct.BE.get_uint16 buffer offset in
      do_checksum checksum (offset + 2)
    ) else if offset + 1 = len then (
      let checksum = checksum + (Cstruct.get_uint8 buffer offset lsl 8) in
      finalise_checksum checksum
    ) else
      finalise_checksum checksum
  in
  lnot (do_checksum 0 0) land 0xffff

let ones_complement_list buffers =
  ones_complement (Cstruct.concat buffers)
