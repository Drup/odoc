open Result
open Odoc_model.Paths
module Url = Odoc_document.Url

type t = Url.t = {
  page : string list;
  anchor : string;
  kind : string;
}

module Anchor = struct
  type t = {
    kind : string;
    name : string;
  }

  module Polymorphic_variant_decl = struct
    let name_of_type_constr te =
      match te with
      | Odoc_model.Lang.TypeExpr.Constr (path, _) -> Url.render_path (path :> Odoc_model.Paths.Path.t)
      | _ ->
        invalid_arg "DocOckHtml.Url.Polymorphic_variant_decl.name_of_type_constr"

    let from_element ~type_ident elt =
      match Url.from_identifier ~stop_before:true type_ident with
      | Error e -> failwith (Url.Error.to_string e)
      | Ok { anchor; _ } ->
        match elt with
        | Odoc_model.Lang.TypeExpr.Polymorphic_variant.Type te ->
          { kind = "type"
          ; name = Printf.sprintf "%s.%s" anchor (name_of_type_constr te) }
        | Constructor {name; _} ->
          { kind = "constructor"
          ; name = Printf.sprintf "%s.%s" anchor name }
  end

  module Module_listing = struct
    module Reference = Odoc_model.Paths.Reference

    (* TODO: better error message. *)
    let fail () = failwith "Only modules allowed inside {!modules: ...}"

    let rec from_reference : Reference.t -> t = function
      | `Root (name, _) -> { kind = "xref-unresolved"; name = Odoc_model.Names.UnitName.to_string name }
      | `Dot (parent, suffix) ->
        let { name; _ } = from_reference (parent :> Reference.t) in
        { kind = "xref-unresolved"; name = Printf.sprintf "%s.%s" name suffix }
      | `Module (parent, suffix) ->
        let { name; _ } = from_reference (parent :> Reference.t) in
        { kind = "xref-unresolved"; name = Printf.sprintf "%s.%s" name (Odoc_model.Names.ModuleName.to_string suffix) }
      | `ModuleType (parent, suffix) ->
        let { name; _ } = from_reference (parent :> Reference.t) in
        { kind = "xref-unresolved"; name = Printf.sprintf "%s.%s" name (Odoc_model.Names.ModuleTypeName.to_string suffix) }
      | `Resolved r ->
        from_resolved r
      | _ ->
        fail ()

    and from_resolved : Reference.Resolved.t -> t =
      function
      | `Identifier id ->
        let name = Identifier.name id in
        let kind =
          match Url.from_identifier ~stop_before:false id with
          | Ok { kind; _ } -> kind
          | Error _ -> fail ()
        in
        { name; kind }
      | `Module (parent, s) ->
        let { name; _ } = from_resolved (parent :> Reference.Resolved.t) in
        { kind = "module"; name = Printf.sprintf "%s.%s" name (Odoc_model.Names.ModuleName.to_string s) }
      | `ModuleType (parent, s) ->
        let { name; _ } = from_resolved (parent :> Reference.Resolved.t) in
        { kind = "module-type"; name = Printf.sprintf "%s.%s" name (Odoc_model.Names.ModuleTypeName.to_string s) }
      | _ ->
        fail ()
  end
end
