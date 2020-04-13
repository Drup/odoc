open Odoc_model.Paths

type t = Odoc_document.Url.t = {
  page : string list;
  (** [Foo.Bar.lol] becomes [["lol"; "Bar"; "Foo"]]. *)

  anchor : string;
  (** Anchor in {!page} where the element is attached *)

  kind : string;
  (** What kind of element the path points to.
      e.g. "module", "module-type", "exception", ... *)
}
(** A low level representation of ocaml paths. *)

module Anchor : sig
  type t = {
    kind : string;
    name : string;
  }

  module Polymorphic_variant_decl : sig
    val from_element
      : type_ident:Identifier.t
      -> Odoc_model.Lang.TypeExpr.Polymorphic_variant.element
      -> t
  end

  module Module_listing : sig
    val from_reference : Reference.t -> t
  end
end
