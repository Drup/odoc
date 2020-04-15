(*
 * Copyright (c) 2016 Thomas Refis <trefis@janestreet.com>
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



module Html = Tyxml.Html
module Url = Odoc_document.Url

(** Supported languages for printing code parts. *)

type syntax = OCaml | Reason

val string_of_syntax: syntax -> string

type t

val traverse
    : f:(parents:string list -> string -> [ `Html ] Html.elt -> unit)
  -> t
  -> unit

type uri =
  | Absolute of string
  | Relative of string
(** The type for absolute and relative URIs. The relative URIs are resolved
    using the HTML output directory as a target. *)

(** {1 Page creator} *)

(* TOOD Passing the header docs should become non-optional as the code in
   To_html_tree is progressively refactored. *)
val make :
  ?header_docs:(Html_types.flow5_without_header_footer Html.elt) list ->
  ?theme_uri:uri ->
  url:Url.Path.t ->
  string ->
  (Html_types.div_content Html.elt) list ->
  t list ->
    t
(** [make ?theme_uri (body, children)] calls "the page creator" to turn [body]
    into an [[ `Html ] elt]. If [theme_uri] is provided, it will be used to
    locate the theme files, otherwise the HTML output directory is used. *)

module Link : sig
  val semantic_uris : bool ref
  (** Whether to generate pretty/semantics links or not. *)

  type t =
    | Current of Url.Path.t
    | Base of string
  
  val href : resolve:t -> Url.t -> string

end

(* val render_fragment : Paths.Fragment.t -> string *)

(* TODO: move to a centralized [State] module or something. Along with
   Relative_link.semantic_uris. *)
val open_details : bool ref
(** Whether [<details>] tags should be opened by default or not.
    Default is [true]. *)
