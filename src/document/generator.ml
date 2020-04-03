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


open Odoc_model.Names
module Location = Odoc_model.Location_
module Paths = Odoc_model.Paths
open Types

let functor_arg_pos { Odoc_model.Lang.FunctorParameter.id ; _ } =
  match id with
  | `Argument (_, nb, _) -> nb
  | _ ->
    failwith "TODO"
    (* let id = string_of_sexp @@ Identifier.sexp_of_t id in
    invalid_arg (Printf.sprintf "functor_arg_pos: %s" id) *)

module O = struct

  type out = Source.t

  type Format.stag +=
    | Elt of Inline.t

  let make () =
    let open Inline in
    let out : out ref = ref [] in
    let opened_tag : Source.decoration Stack.t = Stack.create () in
    let push elt = out := (Stack.top_opt opened_tag, elt) :: !out in
    let out_functions = {Format.
      out_string = (fun s i j -> push [Text (String.sub s i j)]);
      out_flush = (fun () -> ());
      out_newline = (fun () -> push [Linebreak]);
      out_spaces = (fun n -> push [Text (String.make n ' ')]);
      out_indent = (fun n -> push [Text (String.make n ' ')])
    }
    and stag_functions =
      let print_open_stag = function
        | Elt elt -> push elt
        | Format.String_tag s -> Stack.push s opened_tag
        | _ -> ()
      and print_close_stag = function
        | Elt _ -> ()
        | Format.String_tag _ -> ignore (Stack.pop_opt opened_tag)
        | _ -> ()
      in {Format.
        mark_open_stag = (fun _ -> "");
        mark_close_stag = (fun _ -> "");
        print_open_stag; print_close_stag;
      }
    in
    let formatter = Format.formatter_of_out_functions out_functions in
    Format.pp_set_formatter_stag_functions formatter stag_functions;
    (fun () -> !out), formatter

  let elt ppf e = Format.pp_open_stag ppf (Elt e); Format.pp_close_stag ppf ()

  let entity ppf e = elt ppf [Inline.Entity e]

  let spf fmt =
    let flush, ppf = make () in
    Format.kfprintf (fun _ -> flush ()) ppf fmt
  
  let pf = Format.fprintf
  
  (** Transitory hackish API *)
      
  let (++) f g ppf = f ppf; g ppf
  let span f ppf = pf ppf "@[%t@]" f
  let df = Format.dprintf
  let txt s ppf = Format.pp_print_string ppf s
  let noop (_ : Format.formatter) = ()

  let (!) (pp : _ Fmt.t) x ppf = pp ppf x

  let rec list ?sep ~f = function
    | [] -> noop
    | [x] -> f x
    | x :: xs ->
      let hd = f x in
      let tl = list ?sep ~f xs in
      match sep with
      | None -> hd ++ tl
      | Some sep -> hd ++ sep ++ tl

  let code f =
    [Inline.Source (spf "%t" f)]
end
open O

let label = function
  | Odoc_model.Lang.TypeExpr.Label s -> O.df "%s" s
  | Optional s -> O.df "?%a%s" O.entity "#8288" s

let keyword keyword =
  O.df "@[<keyword>%s@]" keyword

let type_var tv =
  O.df "@[<type-var>%s@]" tv

let enclose ~l ~r x =
  O.df "%s%t%s" l x r

let resolved p txt =
  O.df "%a" O.elt [Inline.InternalLink (InternalLink.Resolved (p, txt))]
let unresolved txt = 
  O.df "%a" O.elt [Inline.InternalLink (InternalLink.Unresolved txt)]

include Generator_signatures


type text = Format.formatter -> unit


(**
   Main functor to create an {!To_html_tree.Html_generator}
 *)
module Make (Syntax : SYNTAX) = struct

module Link :
sig
  val from_path : stop_before:bool -> Paths.Path.t -> text
  val from_fragment :
    base:Paths.Identifier.Signature.t -> Paths.Fragment.t -> text
  val render_fragment : Paths.Fragment.t -> string
end =
struct
  open Paths
  
  let rec from_path : stop_before:bool -> Path.t -> text =
    fun ~stop_before path ->
      match path with
      | `Root root -> unresolved [Text root]
      | `Forward root -> unresolved [Text root] (* FIXME *)
      | `Dot (prefix, suffix) ->
        let link = from_path ~stop_before:true (prefix :> Path.t) in
        link ++ O.txt ("." ^ suffix) 
      | `Apply (p1, p2) ->
        let link1 = from_path ~stop_before (p1 :> Path.t) in
        let link2 = from_path ~stop_before (p2 :> Path.t) in
        link1 ++ O.txt "(" ++ link2 ++ O.txt ")"
      | `Resolved rp ->
        let id = Paths.Path.Resolved.identifier rp in
        let txt = Url.render_path path in
        begin match Url.from_identifier ~stop_before id with
        | Ok href ->
          resolved href [Text txt]
        | Error Url.Error.Not_linkable _ -> O.txt txt
        | Error exn ->
          Printf.eprintf "Id.href failed: %S\n%!" (Url.Error.to_string exn);
          O.txt txt
        end

  let dot prefix suffix =
    match prefix with
    | "" -> suffix
    | _  -> prefix ^ "." ^ suffix

  let rec render_fragment : Fragment.t -> string =
    fun fragment ->
    match fragment with
    | `Resolved rr -> render_resolved_fragment rr
    | `Dot (prefix, suffix) ->
      dot (render_fragment (prefix :> Fragment.t)) suffix

  and render_resolved_fragment : Fragment.Resolved.t -> string =
    let open Fragment.Resolved in
    fun fragment ->
      match fragment with
      | `Root -> ""
      | `Subst (_, rr) -> render_resolved_fragment (rr :> t)
      | `SubstAlias (_, rr) -> render_resolved_fragment (rr :> t)
      | `Module (rr, s) ->
        dot (render_resolved_fragment (rr :> t)) (ModuleName.to_string s)
      | `Type (rr, s) ->
        dot (render_resolved_fragment (rr :> t)) (TypeName.to_string s)
      | `Class (rr, s) ->
        dot (render_resolved_fragment ( rr :> t)) (ClassName.to_string s)
      | `ClassType (rr, s) ->
        dot (render_resolved_fragment (rr :> t)) (ClassTypeName.to_string s)

  let rec fragment_to_ir : stop_before:bool ->
    base:Identifier.Signature.t -> Fragment.t -> text =
    fun ~stop_before ~base fragment ->
    let open Fragment in
    match fragment with
    | `Resolved `Root ->
      let id = (base :> Identifier.t) in
      begin match Url.from_identifier ~stop_before:true id with
      | Ok href ->
        resolved href [Inline.Text (Identifier.name id)]
      | Error (Not_linkable _) ->
        unresolved [Inline.Text (Identifier.name id)]
      | Error exn ->
        Printf.eprintf "[FRAG] Id.href failed: %S\n%!"
          (Url.Error.to_string exn);
        unresolved [Inline.Text (Identifier.name id)]
      end
    | `Resolved rr ->
      let id = Resolved.identifier base (rr :> Resolved.t) in
      let txt = render_resolved_fragment rr in
      begin match Url.from_identifier ~stop_before id with
      | Ok href ->
        resolved href [Inline.Text txt]
      | Error (Not_linkable _) ->
        unresolved [Inline.Text txt]
      | Error exn ->
        Printf.eprintf "[FRAG] Id.href failed: %S\n%!"
          (Url.Error.to_string exn);
        unresolved [Inline.Text txt]
      end
    | `Dot (prefix, suffix) ->
      let link = fragment_to_ir ~stop_before:true ~base (prefix :> Fragment.t) in
      link ++ O.txt ("." ^ suffix)

  let from_fragment = fragment_to_ir ~stop_before:false

end

  
module Type_expression :
sig
  val type_expr : ?needs_parentheses:bool -> Lang.TypeExpr.t -> text

  val format_type_path :
    delim:[ `parens | `brackets ] -> Lang.TypeExpr.t list -> text -> text
end =
struct
  let rec te_variant (t : Odoc_model.Lang.TypeExpr.Polymorphic_variant.t) =
    let style_arguments ~constant arguments =
      (* Multiple arguments in a polymorphic variant constructor correspond
         to a conjunction of types, not a product: [`Lbl int&float].
         If constant is [true], the conjunction starts with an empty type,
         for instance [`Lbl &int].
      *)
      let wrapped_type_expr =
        (* type conjunction in Reason is printed as `Lbl (t1)&(t2)` *)
        if Syntax.Type.Variant.parenthesize_params then
          fun x ->
            O.span (O.txt "(" ++ type_expr x ++ O.txt ")")
        else
          fun x -> type_expr x
      in
      let arguments =
        O.list
          arguments
          ~sep:(O.txt " & ")
          ~f:wrapped_type_expr
      in
      if constant
      then O.txt "& " ++ arguments
      else arguments
    in
    let rec style_elements ~add_pipe = function
      | [] -> O.noop
      | first :: rest ->
        let first = 
          match first with
          | Odoc_model.Lang.TypeExpr.Polymorphic_variant.Type te ->
            let res = type_expr te in
            if add_pipe
            then O.txt " " ++ O.span (O.txt "| " ++ res)
            else res 
          | Constructor {constant; name; arguments; _} ->
            let constr =
              let name = "`" ^ name in
              if add_pipe
              then O.span (O.txt ("| " ^ name))
              else O.txt name
            in
            let res =
              match arguments with
              | [] -> constr
              | _ -> 
                let arguments = style_arguments ~constant arguments in
                O.span (
                  if Syntax.Type.Variant.parenthesize_params
                  then constr ++ arguments
                  else constr ++ O.txt  " of " ++ arguments
                )
            in
            if add_pipe
            then O.txt " " ++ res
            else res 
        in
        first ++ style_elements ~add_pipe:true rest
    in
    let elements = style_elements ~add_pipe:false t.elements in
    O.span (
       match t.kind with
       | Fixed -> O.txt "[ " ++ elements ++ O.txt " ]"
       | Open -> O.txt "[> " ++ elements ++ O.txt " ]"
       | Closed [] -> O.txt "[< " ++ elements ++ O.txt " ]"
       | Closed lst ->
         let constrs = String.concat " " lst in
         O.txt "[< " ++ elements ++ (O.txt (" " ^ constrs ^ " ]"))
     )

  and te_object (t : Odoc_model.Lang.TypeExpr.Object.t) =
    let fields =
      O.list t.fields ~f:(function
        | Odoc_model.Lang.TypeExpr.Object.Method {name; type_} ->
          O.txt (name ^ Syntax.Type.annotation_separator)
          ++ type_expr type_
          ++ O.txt Syntax.Obj.field_separator
        | Inherit type_ ->
            type_expr type_ ++ O.txt Syntax.Obj.field_separator)
    in
    let open_tag =
        if t.open_ then O.txt Syntax.Obj.open_tag_extendable
        else O.txt Syntax.Obj.open_tag_closed
    in
    let close_tag =
        if t.open_ then O.txt Syntax.Obj.close_tag_extendable
        else O.txt Syntax.Obj.close_tag_closed
    in
    open_tag ++ fields ++ close_tag

  and format_type_path
        ~delim (params : Odoc_model.Lang.TypeExpr.t list) (path : text) : text =
    match params with
    | [] -> path
    | [param] ->
        let param = (type_expr ~needs_parentheses:true param) in
        let args =
          if Syntax.Type.parenthesize_constructor
          then  O.txt "(" ++ param ++ O.txt ")"
          else param
        in
      Syntax.Type.handle_constructor_params path args
    | params  ->
      let params =
        O.list params ~sep:(O.txt ",\194\160")
          ~f:type_expr
      in
      let params = match delim with
        | `parens   -> enclose ~l:"(" params ~r:")"
        | `brackets -> enclose ~l:"[" params ~r:"]"
      in
      Syntax.Type.handle_constructor_params path params

  and type_expr 
        ?(needs_parentheses=false) (t : Odoc_model.Lang.TypeExpr.t) =
    match t with
    | Var s -> type_var (Syntax.Type.var_prefix ^ s)
    | Any  -> type_var Syntax.Type.any
    | Alias (te, alias) ->
      type_expr ~needs_parentheses:true te ++
      O.txt " " ++ keyword "as" ++ O.txt " '" ++ O.txt alias
    | Arrow (None, src, dst) ->
      let res =
        type_expr ~needs_parentheses:true src ++
        O.txt " " ++ Syntax.Type.arrow ++ O.txt " " ++ type_expr dst
      in
      if not needs_parentheses then
        res
      else
        enclose ~l:"(" res ~r:")"
    | Arrow (Some lbl, src, dst) ->
      let res =
        O.span (
          label lbl ++ O.txt ":" ++ type_expr ~needs_parentheses:true src
        ) ++ O.txt " " ++ Syntax.Type.arrow ++ O.txt " " ++ type_expr dst
      in
      if not needs_parentheses then
        res
      else
        enclose ~l:"(" res ~r:")"
    | Tuple lst ->
      let res =
        O.list
          lst
          ~sep:(O.txt Syntax.Type.Tuple.element_separator)
          ~f:(type_expr ~needs_parentheses:true)
      in
      if Syntax.Type.Tuple.always_parenthesize || needs_parentheses then
        enclose ~l:"(" res ~r:")"
      else
        res
    | Constr (path, args) ->
      let link = Link.from_path ~stop_before:false (path :> Paths.Path.t) in
      format_type_path ~delim:(`parens) args link
    | Polymorphic_variant v -> te_variant v
    | Object o -> te_object o
    | Class (path, args) ->
      format_type_path ~delim:(`brackets) args
        (Link.from_path ~stop_before:false (path :> Paths.Path.t))
    | Poly (polyvars, t) ->
      O.txt (String.concat " " polyvars ^ ". ") ++ type_expr t
    | Package pkg ->
      enclose ~l:"(" ~r:")" (
         keyword "module" ++ O.txt " " ++
           Link.from_path ~stop_before:false (pkg.path :> Paths.Path.t) ++
         match pkg.substitutions with
         | [] -> O.noop
         | lst ->
           O.txt " " ++ keyword "with" ++ O.txt " " ++
           O.list
             ~sep:(O.txt " " ++ keyword "and" ++ O.txt " ")
             lst
             ~f:(package_subst pkg.path)
       )

  and package_subst (pkg_path : Paths.Path.ModuleType.t)
        (frag_typ, te : Paths.Fragment.Type.t * Odoc_model.Lang.TypeExpr.t)
    : text =
    keyword "type" ++
    O.txt " " ++
    (match pkg_path with
    | `Resolved rp ->
      let base = (Paths.Path.Resolved.ModuleType.identifier rp :> Paths.Identifier.Signature.t) in
      Link.from_fragment ~base
        (frag_typ :> Paths.Fragment.t)
    | _ ->
      O.txt
        (Link.render_fragment (frag_typ :> Paths.Fragment.t))) ++
    O.txt " = " ++
    type_expr te
end
open Type_expression



(* Also handles constructor declarations for exceptions and extensible
   variants, and exposes a few helpers used in formatting classes and signature
   constraints. *)
module Type_declaration :
sig
  val type_decl :
    ?is_substitution:bool -> Lang.Signature.recursive * Lang.TypeDecl.t ->
      rendered_item * Odoc_model.Comment.docs
  val extension : Lang.Extension.t -> rendered_item * Odoc_model.Comment.docs
  val exn : Lang.Exception.t -> rendered_item * Odoc_model.Comment.docs

  val format_params :
    ?delim:[ `parens | `brackets ] ->
    Lang.TypeDecl.param list -> text

  val format_manifest : ?is_substitution:bool -> ?compact_variants:bool -> Lang.TypeDecl.Equation.t -> text * bool

  val format_constraints : (Lang.TypeExpr.t * Lang.TypeExpr.t) list -> text
end =
struct
  let record fields =
    let field mutable_ id typ =
      match Url.from_identifier ~stop_before:true id with
      | Error e -> failwith (Url.Error.to_string e)
      | Ok { anchor; kind; _ } ->
        let name = Paths.Identifier.name id in
        let cell =
          O.td ~a:[ O.a_class ["def"; kind ] ]
            [O.a ~a:[O.a_href ("#" ^ anchor); O.a_class ["anchor"]] []
            ; O.code (
                (if mutable_ then keyword "mutable" ++ O.txt " " else O.noop)
                ++ O.txt name
                ++ O.txt Syntax.Type.annotation_separator
                ++ type_expr typ
                ++ O.txt Syntax.Type.Record.field_separator
              )
            ]
        in
        anchor, cell
    in
    let rows =
      fields |> List.map (fun fld ->
        let open Odoc_model.Lang.TypeDecl.Field in
        let anchor, lhs =
          field fld.mutable_ (fld.id :> Paths.Identifier.t) fld.type_
        in 
        let rhs = Comment.to_ir fld.doc in
        O.tr ~a:[ O.a_id anchor; O.a_class ["anchored"] ] (
          lhs ++
          if not (Comment.has_doc fld.doc) then [] else [
            O.td ~a:[ O.a_class ["doc"] ] rhs
          ]
        )
      )
    in
    O.code (O.txt "{")
    @ O.table ~a:[ O.a_class ["record"] ] rows
    @ O.code (O.txt "}")
 


  let constructor
    : Paths.Identifier.t -> Odoc_model.Lang.TypeDecl.Constructor.argument
    -> Odoc_model.Lang.TypeExpr.t option
    -> [> `Code | `PCDATA | `Table ] O.elt list
  = fun id args ret_type ->
      let name = Paths.Identifier.name id in
      let cstr =
        O.span
          ~a:[O.a_class [Url.kind_of_id_exn id]] [O.txt name]
      in
      let is_gadt, ret_type =
        match ret_type with
        | None -> false, []
        | Some te ->
          let constant =
            match args with
            | Tuple [] -> true
            | _ -> false
          in
          let ret_type =
            O.txt " " ++
            (if constant then O.txt ":" else Syntax.Type.GADT.arrow) ++
            O.txt " " ++
            type_expr te
          in
          true, ret_type
      in
      match args with
      | Tuple [] -> [ O.code (cstr ++ ret_type) ]
      | Tuple lst ->
        let params = O.list lst
          ~sep:(O.txt Syntax.Type.Tuple.element_separator)
          ~f:(type_expr ~needs_parentheses:is_gadt)
        in
        [ O.code (
            cstr ++
            (
              if Syntax.Type.Variant.parenthesize_params
              then O.txt "(" ++ params ++ [ O.txt ")" ]
              else
                (if is_gadt then
                  [O.txt Syntax.Type.annotation_separator]
                else
                  [O.txt " "; keyword "of"; O.txt " "]) ++
                params
            )
            ++ ret_type
          )
        ]
      | Record fields ->
        if is_gadt then
          (O.code [cstr; O.txt Syntax.Type.annotation_separator])
          ++(record fields)
          ++ [O.code ret_type]
        else
          (O.code
            [cstr; O.txt " "; keyword "of"; O.txt " "])++(record fields)



  let variant cstrs : [> Html_types.table ] O.elt =
    let constructor id args res =
      match Url.from_identifier ~stop_before:true id with
      | Error e -> failwith (Url.Error.to_string e)
      | Ok { anchor; kind; _ } ->
        let cell =
          O.td ~a:[ O.a_class ["def"; kind ] ] (
            O.a ~a:[O.a_href ("#" ^ anchor); O.a_class ["anchor"]]
              [] ++
            O.code [O.txt "| " ] ++
            constructor id args res
          )
        in
        anchor, cell
    in
    match cstrs with
    | [] -> O.code [ O.txt "|" ]
    | _ :: _ ->
      let rows =
        cstrs |> List.map (fun cstr ->
          let open Odoc_model.Lang.TypeDecl.Constructor in
          let anchor, lhs = constructor (cstr.id :> Paths.Identifier.t) cstr.args cstr.res in
          let rhs = Comment.to_html cstr.doc in
          let rhs = (rhs :> (Html_types.td_content O.elt) list) in
          O.tr ~a:[ O.a_id anchor; O.a_class ["anchored"] ] (
            lhs ++
            if not (Comment.has_doc cstr.doc) then [] else [
              O.td ~a:[ O.a_class ["doc"] ] rhs
            ]
          )
        )
      in
      O.table ~a:[ O.a_class ["variant"] ] rows



  let extension_constructor (t : Odoc_model.Lang.Extension.Constructor.t) =
    (* TODO doc *)
    constructor (t.id :> Paths.Identifier.t) t.args t.res

  let extension (t : Odoc_model.Lang.Extension.t) =
    let extension =
      O.code (
        keyword "type" ++
        O.txt " " ++
        Tree.Relative_link.of_path ~stop_before:false (t.type_path :> Paths.Path.t) ++
        [ O.txt " += " ]
      ) ++
      O.list t.constructors ~sep:(O.code [O.txt " | "])
        ~f:extension_constructor
      ++ (if Syntax.Type.type_def_semicolon then [ O.txt ";" ] else [])
    in
    extension, t.doc



  let exn (t : Odoc_model.Lang.Exception.t) =
    let cstr = constructor (t.id :> Paths.Identifier.t) t.args t.res in
    let exn = O.code [ keyword "exception"; O.txt " " ] ++ cstr
      ++ (if Syntax.Type.Exception.semicolon then [ O.txt ";" ] else [])
    in
    exn, t.doc



  let polymorphic_variant
      ~type_ident (t : Odoc_model.Lang.TypeExpr.Polymorphic_variant.t) =

    let row item =
      let kind_approx, cstr, doc =
        match item with
        | Odoc_model.Lang.TypeExpr.Polymorphic_variant.Type te ->
          "unknown", [O.code (type_expr te)], None
        | Constructor {constant; name; arguments; doc; _} ->
          let cstr = "`" ^ name in
          "constructor",
          begin match arguments with
          | [] -> [O.code [ O.txt cstr ]]
          | _ ->
            (* Multiple arguments in a polymorphic variant constructor correspond
               to a conjunction of types, not a product: [`Lbl int&float].
               If constant is [true], the conjunction starts with an empty type,
               for instance [`Lbl &int].
            *)
            let wrapped_type_expr =
              (* type conjunction in Reason is printed as `Lbl (t1)&(t2)` *)
              if Syntax.Type.Variant.parenthesize_params then
                fun x -> O.txt "(" ++ type_expr x ++ [O.txt ")"]
              else
                fun x -> type_expr x
            in
            let params = O.list arguments
              ~sep:(O.txt " & ")
              ~f:wrapped_type_expr
            in
            let params =
              if constant then O.txt "& " ++ params else params in
            [ O.code (
                O.txt cstr ++
                (
                if Syntax.Type.Variant.parenthesize_params
                then params
                else O.txt " " ++ keyword "of" ++ O.txt " " ++ params
                )
              )
            ]
          end,
          match doc with
          | [] ->
            None
          | _ ->
            Some (Comment.to_html doc :> (Html_types.td_content O.elt) list)
      in
      try
        let { Url.Anchor. name = anchor; kind } =
          Url.Anchor.Polymorphic_variant_decl.from_element ~type_ident item
        in
        let constructor_column =
          O.td ~a:[ O.a_class ["def"; kind] ] (
            O.a ~a:[
              Tyxml.O.a_href ("#" ^ anchor); O.a_class ["anchor"] ] [] ++
            O.code [O.txt "| " ] ++
            cstr)
        in
        let columns =
          match doc with
          | None ->
            [constructor_column]
          | Some doc ->
            [constructor_column; O.td ~a:[O.a_class ["doc"]] doc]
        in
        O.tr ~a:[ O.a_id anchor; O.a_class ["anchored"] ] columns
      with Failure s ->
        Printf.eprintf "ERROR: %s\n%!" s;
        O.tr [
          O.td ~a:[ O.a_class ["def"; kind_approx] ] (
            O.code [O.txt "| " ] ++
            cstr
          );
        ]
    in
    let table =
      O.table ~a:[O.a_class ["variant"]] (List.map row t.elements) in
    match t.kind with
    | Fixed ->
      O.code [O.txt "[ "] ++ table ++ [O.code [O.txt " ]"]]
    | Open ->
      O.code [O.txt "[> "] ++ table ++ [O.code [O.txt " ]"]]
    | Closed [] ->
      O.code [O.txt "[< "] ++ table ++ [O.code [O.txt " ]"]]
    | Closed lst ->
      let constrs = String.concat " " lst in
      O.code [O.txt "[< "] ++ table ++
        [O.code [O.txt (" " ^ constrs ^ " ]")]]

 

  let format_params
    : 'row. ?delim:[`parens | `brackets] -> Odoc_model.Lang.TypeDecl.param list
    -> Inline.t
  = fun ?(delim=`parens) params ->
    let format_param (desc, variance_opt) =
      let param_desc =
        match desc with
        | Odoc_model.Lang.TypeDecl.Any -> "_"
        | Var s -> "'" ^ s
      in
      match variance_opt with
      | None -> param_desc
      | Some Odoc_model.Lang.TypeDecl.Pos -> "+" ^ param_desc
      | Some Odoc_model.Lang.TypeDecl.Neg -> "-" ^ param_desc
    in
    O.txt (
      match params with
      | [] -> ""
      | [x] -> format_param x |> Syntax.Type.handle_format_params
      | lst ->
        let params = String.concat ", " (List.map format_param lst) in
        (match delim with `parens -> "(" | `brackets -> "[")
        ^ params ^
        (match delim with `parens -> ")" | `brackets -> "]")
    )

  let format_constraints constraints =
    O.list constraints ~f:begin fun (t1, t2) ->
      O.txt " " ++
      keyword "constraint" ++
      O.txt " " ++
      type_expr t1 ++
      O.txt " = " ++
      type_expr t2
    end

  let format_manifest
    : 'inner_row 'outer_row. ?is_substitution:bool -> ?compact_variants:bool
    -> Odoc_model.Lang.TypeDecl.Equation.t
    -> text * bool
  = fun ?(is_substitution=false) ?(compact_variants=true) equation ->
    let _ = compact_variants in (* TODO *)
    let private_ = equation.private_ in
    match equation.manifest with
    | None -> [], private_
    | Some t ->
      let manifest =
        O.txt (if is_substitution then " := " else " = ") ++
        (if private_ then
          [keyword Syntax.Type.private_keyword; O.txt " "]
        else []) ++
        type_expr t
      in
      manifest, false



  let type_decl ?(is_substitution=false) ((recursive, t) : Lang.Signature.recursive * Lang.TypeDecl.t) =
    let tyname = Paths.Identifier.name t.id in
    let params = format_params t.equation.params in
    let constraints = format_constraints t.equation.constraints in
    let manifest, need_private =
      match t.equation.manifest with
      | Some (Odoc_model.Lang.TypeExpr.Polymorphic_variant variant) ->
        let manifest =
          (O.txt (if is_substitution then " := " else " = ") ++
          if t.equation.private_ then
            [keyword Syntax.Type.private_keyword; O.txt " "]
          else
            []) ++
          polymorphic_variant ~type_ident:(t.id :> Paths.Identifier.t) variant
        in
        manifest, false
      | _ ->
        let manifest, need_private = format_manifest ~is_substitution t.equation in
        Utils.optional_code manifest, need_private
    in
    let representation = 
      match t.representation with
      | None -> []
      | Some repr ->
        O.code (
          O.txt " = " ++
          if need_private then
            keyword Syntax.Type.private_keyword ++ O.txt " "
          else
            O.noop
        ) @
        match repr with
        | Extensible -> O.code (O.txt "..") 
        | Variant cstrs -> [variant cstrs]
        | Record fields -> record fields
    in
    let tdecl_def =
      let keyword' =
        match recursive with
        | Ordinary | Rec -> [keyword "type"]
        | And -> [keyword "and"]
        | Nonrec -> [keyword "type"; O.txt " "; keyword "nonrec"]
      in

      O.code (
          keyword' ++ O.txt " "
          ++ (Syntax.Type.handle_constructor_params [O.txt tyname] [params])
        ) ::
        O.spf
          "%t%t%t%s"
          manifest
          representation
          Utils.optional_code constraints
          (if Syntax.Type.type_def_semicolon then ";" else "")
    in
    tdecl_def, t.doc
end
open Type_declaration



module Value :
sig
  val value : Lang.Value.t -> rendered_item * Odoc_model.Comment.docs
  val external_ : Lang.External.t -> rendered_item * Odoc_model.Comment.docs
end =
struct
  let value (t : Odoc_model.Lang.Value.t) =
    let name = Paths.Identifier.name t.id in
    let value =
      keyword Syntax.Value.variable_keyword ++
      O.txt " " ++
      O.txt name ++
      O.txt Syntax.Type.annotation_separator ++
      type_expr t.type_
      ++ (if Syntax.Value.semicolon then [ O.txt ";" ] else [])
    in
    [O.code value], t.doc

  let external_ (t : Odoc_model.Lang.External.t) =
    let name = Paths.Identifier.name t.id in
    let external_ =
      keyword Syntax.Value.variable_keyword ++
      O.txt " " ++
      O.txt name ++
      O.txt Syntax.Type.annotation_separator ++
      type_expr t.type_
      ++ (if Syntax.Type.External.semicolon then [ O.txt ";" ] else [])
    in
    [O.code external_], t.doc
end
open Value

module ModuleSubstitution :
sig
  val module_substitution : Lang.ModuleSubstitution.t -> rendered_item * Odoc_model.Comment.docs
end =
struct
  let module_substitution (t : Odoc_model.Lang.ModuleSubstitution.t) =
    let name = Paths.Identifier.name t.id in
    let path = Tree.Relative_link.of_path ~stop_before:true (t.manifest :> Paths.Path.t) in
    let value =
      keyword "module" ++
      O.txt " " ++
      O.txt name ++
      O.txt " := " ++
      path
    in
    [O.code value], t.doc
end
open ModuleSubstitution


(* This chunk of code is responsible for laying out signatures and class
   signatures: the parts of OCaml that contain other parts as nested items.

   Each item is either

   - a leaf, like a type declaration or a value,
   - something that has a nested signature/class signature, or
   - a comment.

   Comments can contain section headings, and the top-level markup code is also
   responsible for generating a table of contents. As a result, it must compute
   the nesting of sections.

   This is also a good opportunity to properly nest everything in <section>
   tags. Even though that is not strictly required by HTML, we carry out the
   computation for it anyway when computing nesting for the table of
   contents.

   Leaf items are set in <dl> tags – the name and any definition in <dt>, and
   documentation in <dd>. Multiple adjacent undocumented leaf items of the same
   kind are set as sibling <dt>s in one <dl>, until one of them has
   documentation. This indicates groups like:

{[
type sync
type async
(** Documentation for both types. *)
]}

   Nested signatures are currently marked up with <article> tags. The top-level
   layout code is eventually indirectly triggered recursively for laying them
   out, as well. *)

module Top_level_markup :
sig
  type heading_level_shift

  val lay_out :
    heading_level_shift option ->
    item_to_id:('item -> string option) ->
    item_to_spec:('item -> string option) ->
    render_leaf_item:('item -> rendered_item * Odoc_model.Comment.docs) ->
    render_nested_article:
      (heading_level_shift -> 'item ->
        rendered_item * Odoc_model.Comment.docs * toc * Tree.t list) ->
    ((_, 'item) tagged_item) list ->
      (Html_types.div_content O.elt) list * toc * Tree.t list

  val render_toc :
    toc -> ([> Html_types.flow5_without_header_footer ] O.elt) list

  val lay_out_page : Odoc_model.Comment.docs ->
      ((Html_types.div_content O.elt) list *
       (Html_types.flow5_without_header_footer O.elt) list *
       toc)
end =
struct
  (* Just some type abbreviations. *)
  type html = Html_types.flow5 O.elt
  type comment_html = Html_types.flow5_without_header_footer O.elt



  let add_anchor item_to_id item html =
    match item_to_id item with
    | None ->
      html,
      []
    | Some anchor_text ->
      let anchor =
        O.a
          ~a:[O.a_href ("#" ^ anchor_text); O.a_class ["anchor"]]
          []
      in
      anchor++html,
      [O.a_id anchor_text]


(* Adds spec class to the list of existing item attributes. *)
  let add_spec item_to_spec item a =
    match item_to_spec item with
    | Some spec -> O.a_class ["spec " ^ spec] ++ a
    | None -> a


  (* "Consumes" adjacent leaf items of the same kind, until one is found with
     documentation. Then, joins all their definitions, and the documentation of
     the last item (if any), into a <dl> element. The rendered <dl> element is
     paired with the list of unconsumed items remaining in the input. *)
  let leaf_item_group
      item_to_id item_to_spec render_leaf_item first_item_kind items
      : html * 'item list =

    let rec consume_leaf_items_until_one_is_documented =
        fun items acc ->

      match items with
      | (`Leaf_item (this_item_kind, item))::items
          when this_item_kind = first_item_kind ->

        let html, maybe_docs = render_leaf_item item in
        let html, maybe_id = add_anchor item_to_id item html in
        let a = add_spec item_to_spec item maybe_id in
        let html = O.dt ~a html in
        let acc = html++acc in

        begin match maybe_docs with
        | [] ->
          consume_leaf_items_until_one_is_documented items acc
        | docs ->
          let docs = Comment.to_html docs in
          let docs = (docs :> (Html_types.dd_content O.elt) list) in
          let docs = O.dd docs in
          List.rev (docs++acc), items
        end

      | _ ->
        List.rev acc, items
    in

    let rendered_item_group, remaining_items =
      consume_leaf_items_until_one_is_documented items [] in

    O.dl rendered_item_group, remaining_items



  (* When we encounter a stop comment, [(**/**)], we read everything until the
     next stop comment, if there is one, and discard it. The returned item list
     is the signature items following the next stop comment, if there are
     any. *)
  let rec skip_everything_until_next_stop_comment : 'item list -> 'item list =
    function
    | [] -> []
    | item::items ->
      match item with
      | `Comment `Stop -> items
      | _ -> skip_everything_until_next_stop_comment items



  (* Reads comment content until the next heading, or the end of comment, and
     renders it as HTML. The returned HTML is paired with the remainder of the
     comment, which will either start with the next section heading in the
     comment, or be empty if there are no section headings. *)
  let render_comment_until_heading_or_end
      : Odoc_model.Comment.docs -> comment_html list * Odoc_model.Comment.docs =
      fun docs ->

    let rec scan_comment acc docs =
      match docs with
      | [] -> List.rev acc, docs
      | block::rest ->
        match block.Location.value with
        | `Heading _ -> List.rev acc, docs
        | _ -> scan_comment (block++acc) rest
    in
    let included, remaining = scan_comment [] docs in
    let docs = Comment.to_html included in
    docs, remaining


  type heading_level_shift = int


  (* The sectioning functions take several arguments, and return "modified"
     instances of them as results. So, it is convenient to group them into a
     record type. This is most useful for the return type, as otherwise there is
     no way to give names to its components.

     The components themselves are:

     - The current comment being read. When non-empty, this is progressively
       replaced with its tail until it is exhausted.
     - The general signature items to be read. These are read one at a time when
       there is no current comment. Upon encountering a comment, it becomes the
       "current comment," and the sectioning functions read it one block element
       at a time, scanning for section headings.
     - A reversed accumulator of the rendered signature items.
     - A reversed accumulator of the table of contents.
     - An accumulator of the subpages generated for nested signatures.

     The record is also convenient for passing around the two item-rendering
     functions. *)
  type ('kind, 'item) sectioning_state = {
    input_items : (('kind, 'item) tagged_item) list;
    acc_subpages : Tree.t list;
    comment_state : comment_state;
    item_to_id : 'item -> string option;
    item_to_spec : 'item -> string option;
    render_leaf_item : 'item -> rendered_item * Odoc_model.Comment.docs;
    render_nested_article :
      heading_level_shift -> 'item ->
      rendered_item * Odoc_model.Comment.docs * toc * Tree.t list;
  }


  (* Comment state used to generate HTML and TOC for both mli and mld inputs. *)
  and comment_state = {
    input_comment : Odoc_model.Comment.docs;
    acc_html : html list;
    acc_toc : toc;
  }

  let finish_comment_state (state : comment_state) =
    {state with
      acc_html = List.rev state.acc_html;
      acc_toc = List.rev state.acc_toc;
    }

  let level_to_int = function
    | `Title -> 0
    | `Section -> 1
    | `Subsection -> 2
    | `Subsubsection -> 3
    | `Paragraph -> 4
    | `Subparagraph -> 5

  let shift shift_by level =
    match shift_by with
    | Some i when i > 0 -> begin
        match level_to_int level + i with
        | 0 -> assert false
        | 1 -> `Section
        | 2 -> `Subsection
        | 3 -> `Subsubsection
        | 4 -> `Paragraph
        | n ->
          assert (n >= 5);
          `Subparagraph
      end
    | None | Some _ -> level

  let is_deeper_section_level other_level ~than =
    level_to_int other_level > level_to_int than


  let rec section_items level_shift section_level state =
    match state.input_items with
    | [] ->
      {state with comment_state =
        finish_comment_state state.comment_state }

    | tagged_item::input_items ->
      match tagged_item with
      | `Leaf_item (kind, _) ->
        let html, input_items =
          leaf_item_group
            state.item_to_id
            state.item_to_spec
            state.render_leaf_item
            kind
            state.input_items
        in
        section_items level_shift section_level {state with
            input_items;
            comment_state = { state.comment_state with
              acc_html = html++state.comment_state.acc_html };
          }

      | `Nested_article item ->
        let html, maybe_docs, toc, subpages =
          state.render_nested_article (level_to_int section_level) item
        in
        let html, maybe_id = add_anchor state.item_to_id item html in
        let a = add_spec state.item_to_spec item maybe_id in
        let html =
          match maybe_docs with
          | [] -> O.div ~a html
          | docs ->
            let docs = Comment.first_to_html docs in
            let docs = (docs :> (Html_types.dd_content O.elt) list) in
            O.dl [O.dt ~a html; O.dd docs]
        in
        section_items level_shift section_level { state with
          input_items;
          comment_state = { state.comment_state with
            acc_html = html++state.comment_state.acc_html;
            acc_toc = List.rev_append toc state.comment_state.acc_toc;
          };
          acc_subpages = state.acc_subpages ++ subpages;
        }

      | `Comment `Stop ->
        let input_items = skip_everything_until_next_stop_comment input_items in
        section_items level_shift section_level {state with
            input_items;
          }

      | `Comment (`Docs input_comment) ->
        section_comment level_shift section_level {state with
            input_items;
            comment_state = { state.comment_state with input_comment };
        }



  and section_comment level_shift section_level state =
    match state.comment_state.input_comment with
    | [] ->
      section_items level_shift section_level state

    | element::input_comment ->

      match element.Location.value with
      | `Heading (level, label, content) ->
        let level = shift level_shift level in
        let element =
          { element with Location.value = `Heading (level, label, content) }
        in
        if not (is_deeper_section_level level ~than:section_level) then
          {state with comment_state =
            finish_comment_state state.comment_state }

        else
          (* We have a deeper section heading in a comment within this section.
             Parse it recursively. We start the nested HTML by parsing the
             section heading itself, and anything that follows it in the current
             comment, up to the next section heading, if any. All of this
             comment matter goes into a <header> element. The nested HTML will
             then be extended recursively by parsing more structure items,
             including, perhaps, additional comments in <aside> elements. *)
          let heading_html = Comment.to_html [element] in
          let more_comment_html, input_comment =
            render_comment_until_heading_or_end input_comment in
          let html = O.header (heading_html ++ more_comment_html) in
          let nested_section_state =
            { state with
              comment_state = {
                input_comment;
                acc_html = [html];
                acc_toc = [];
              }
            }
          in
          let nested_section_state =
            section_comment level_shift level nested_section_state
          in
          (* Wrap the nested section in a <section> element, and extend the
            table of contents. *)
          let html = O.section nested_section_state.comment_state.acc_html in

          let `Label (_, label) = label in
          let toc_entry =
            {
              anchor = Odoc_model.Names.LabelName.to_string label;
              text = content;
              children = nested_section_state.comment_state.acc_toc;
            }
          in

          (* Continue parsing after the nested section. In practice, we have
             either run out of items, or the first thing in the input will be
             another section heading – the nested section will have consumed
             everything else. *)
          section_comment level_shift section_level {nested_section_state with
              comment_state = { nested_section_state.comment_state with
                acc_html = html++state.comment_state.acc_html;
                acc_toc = toc_entry++state.comment_state.acc_toc;
              }
            }

      | _ ->
        let html, input_comment =
          render_comment_until_heading_or_end state.comment_state.input_comment in
        let html = (html :> (Html_types.aside_content O.elt) list) in
        section_comment level_shift section_level {state with
            comment_state = { state.comment_state with
              input_comment;
              acc_html = (O.aside html)++state.comment_state.acc_html;
            }
          }

  let lay_out heading_level_shift ~item_to_id ~item_to_spec
    ~render_leaf_item ~render_nested_article items =
    let initial_state =
      {
        input_items = items;
        comment_state = {
          input_comment = [];
          acc_html = [];
          acc_toc = [];
        };

        acc_subpages = [];

        item_to_id;
        item_to_spec;
        render_leaf_item;
        render_nested_article;
      }
    in
    let state = section_items heading_level_shift `Title initial_state in
    state.comment_state.acc_html, state.comment_state.acc_toc, state.acc_subpages


  let rec page_section_comment ~header_docs section_level state =
    match state.input_comment with
    | [] -> {state with acc_toc = List.rev state.acc_toc}, header_docs
    | element::input_comment ->
      begin match element.Location.value with
      | `Heading (`Title, _label, _content) ->
        let heading_html = Comment.to_html [element] in
        let more_comment_html, input_comment =
          render_comment_until_heading_or_end input_comment in
        let header_docs = heading_html ++ more_comment_html in
        let nested_section_state = {
          input_comment = input_comment;
          acc_html = [];
          acc_toc = [];
        } in
        let nested_section_state, header_docs =
          page_section_comment ~header_docs `Section nested_section_state in
        let acc_html = state.acc_html ++ nested_section_state.acc_html in
        page_section_comment ~header_docs section_level
          { nested_section_state with acc_html }

      | `Heading (level, _label, _content)
        when not (is_deeper_section_level level ~than:section_level) ->
          {state with acc_toc = List.rev state.acc_toc}, header_docs

      | `Heading (level, label, content) ->
        let heading_html = Comment.to_html [element] in
        let more_comment_html, input_comment =
          render_comment_until_heading_or_end input_comment in
        let acc_html =  heading_html ++ more_comment_html in
        let acc_html = (acc_html :> (Html_types.flow5 O.elt) list) in
        let nested_section_state = {
          input_comment = input_comment;
          acc_html;
          acc_toc = [];
        } in
        let nested_section_state, header_docs = page_section_comment ~header_docs level nested_section_state in
        let acc_html = state.acc_html ++ nested_section_state.acc_html in

        let acc_toc =
          let `Label (_, label) = label in
          let toc_entry = {
            anchor = Odoc_model.Names.LabelName.to_string label;
            text = content;
            children = nested_section_state.acc_toc;
          } in
          toc_entry ++ state.acc_toc
        in
        page_section_comment ~header_docs section_level
          { nested_section_state with acc_html; acc_toc }

      | _ ->
        let html, input_comment =
          render_comment_until_heading_or_end state.input_comment in
        let html = (html :> (Html_types.flow5 O.elt) list) in
        page_section_comment ~header_docs section_level {state with
            input_comment;
            acc_html = html ++ state.acc_html;
          }
      end


  let lay_out_page input_comment =
    let initial_state : comment_state = {
      input_comment;
      acc_html = [];
      acc_toc = [];
    } in
    let state, header_docs = page_section_comment ~header_docs:[] `Title initial_state in
    state.acc_html, header_docs, state.acc_toc



  let render_toc toc =
    let rec section the_section : Html_types.li_content O.elt list =
      let text = Comment.link_content_to_html the_section.text in
      let text =
        (text
          : Html_types.phrasing_without_interactive O.elt list
          :> (Html_types.flow5_without_interactive O.elt) list)
      in
      let link =
        O.a
          ~a:[O.a_href ("#" ^ the_section.anchor)] text
      in
      match the_section.children with
      | [] -> [link]
      | _ -> [link; sections the_section.children]

    and sections the_sections =
      the_sections
      |> List.map (fun the_section -> O.li (section the_section))
      |> O.ul

    in

    match toc with
    | [] -> []
    | _ -> [O.nav ~a:[O.a_class ["toc"]] [sections toc]]
end

(* TODO Figure out when this function would fail. It is currently pasted from
   [make_def], but the [make_spec] version doesn't have a [failwith]. *)
let path_to_id path =
  match Url.from_identifier ~stop_before:true path with
  | Error _ ->
    None
  | Ok {anchor; _} ->
    Some anchor



module Class :
sig
  val class_ :
    ?theme_uri:Tree.uri -> Lang.Signature.recursive -> Lang.Class.t ->
      rendered_item * Odoc_model.Comment.docs * toc * Tree.t list

  val class_type :
    ?theme_uri:Tree.uri -> Lang.Signature.recursive -> Lang.ClassType.t ->
      rendered_item * Odoc_model.Comment.docs * toc * Tree.t list
end =
struct
  let class_signature_item_to_id : Lang.ClassSignature.item -> _ = function
    | Method {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | InstanceVariable {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | Constraint _
    | Inherit _
    | Comment _ -> None

  let class_signature_item_to_spec : Lang.ClassSignature.item -> _ = function
    | Method _ -> Some "method"
    | InstanceVariable _ -> Some "instance-variable"
    | Constraint _
    | Inherit _
    | Comment _ -> None

  let tag_class_signature_item : Lang.ClassSignature.item -> _ = fun item ->
    match item with
    | Method _ -> `Leaf_item (`Method, item)
    | InstanceVariable _ -> `Leaf_item (`Variable, item)
    | Constraint _ -> `Leaf_item (`Constraint, item)
    | Inherit _ -> `Leaf_item (`Inherit, item)

    | Comment comment -> `Comment comment

  let rec render_class_signature_item : Lang.ClassSignature.item -> text * _ =
    function
    | Method m -> method_ m
    | InstanceVariable v -> instance_variable v
    | Constraint (t1, t2) -> format_constraints [(t1, t2)], []
    | Inherit (Signature _) -> assert false (* Bold. *)
    | Inherit class_type_expression ->
      [O.code (
        keyword "inherit" ++
        O.txt " " ++
        class_type_expr class_type_expression)],
      []

    | Comment _ -> assert false

  and class_signature (c : Lang.ClassSignature.t) =
    (* FIXME: use [t.self] *)
    let tagged_items = List.map tag_class_signature_item c.items in
    Top_level_markup.lay_out
      None
      ~item_to_id:class_signature_item_to_id
      ~item_to_spec:class_signature_item_to_spec
      ~render_leaf_item:(fun item ->
        let text, docs = render_class_signature_item item in
        (text :> rendered_item), docs
      )
      ~render_nested_article:(fun _ -> assert false)
      tagged_items

  and method_ (t : Odoc_model.Lang.Method.t) =
    let name = Paths.Identifier.name t.id in
    let virtual_ =
      if t.virtual_ then [keyword "virtual"; O.txt " "] else [] in
    let private_ =
      if t.private_ then [keyword "private"; O.txt " "] else [] in
    let method_ =
      keyword "method" ++
      O.txt " " ++
      private_ ++
      virtual_ ++
      O.txt name ++
      O.txt Syntax.Type.annotation_separator ++
      type_expr t.type_
    in
    [O.code method_], t.doc

  and instance_variable (t : Odoc_model.Lang.InstanceVariable.t) =
    let name = Paths.Identifier.name t.id in
    let virtual_ =
      if t.virtual_ then [keyword "virtual"; O.txt " "] else [] in
    let mutable_ =
      if t.mutable_ then [keyword "mutable"; O.txt " "] else [] in
    let val_ =
      keyword "val" ++
      O.txt " " ++
      mutable_ ++
      virtual_ ++
      O.txt name ++
      O.txt Syntax.Type.annotation_separator ++
      type_expr t.type_
    in
    [O.code val_], t.doc

  and class_type_expr (cte : Odoc_model.Lang.ClassType.expr) =
    match cte with
    | Constr (path, args) ->
      let link = Tree.Relative_link.of_path ~stop_before:false (path :> Paths.Path.t) in
      format_type_path ~delim:(`brackets) args link
    | Signature _ ->
      [
        Syntax.Class.open_tag;
        O.txt " ... ";
        Syntax.Class.close_tag
      ]

  and class_decl (cd : Odoc_model.Lang.Class.decl) =
    match cd with
    | ClassType expr -> class_type_expr expr
    (* TODO: factorize the following with [type_expr] *)
    | Arrow (None, src, dst) ->
      type_expr ~needs_parentheses:true src ++
      O.txt " " ++ Syntax.Type.arrow ++ O.txt " " ++ class_decl dst
    | Arrow (Some lbl, src, dst) ->
      label lbl ++ O.txt ":" ++
                  type_expr ~needs_parentheses:true src ++
      O.txt " " ++ Syntax.Type.arrow ++ O.txt " " ++ class_decl dst

  and class_ ?theme_uri recursive (t : Odoc_model.Lang.Class.t) =
    let name = Paths.Identifier.name t.id in
    let params = format_params ~delim:(`brackets) t.params in
    let virtual_ =
      if t.virtual_ then [keyword "virtual"; O.txt " "] else [] in
    let cd = class_decl t.type_ in
    let cname, subtree =
      match t.expansion with
      | None -> O.txt name, []
      | Some csig ->
        Tree.enter ~kind:(`Class) name;
        let doc = Comment.to_html t.doc in
        let expansion, toc, _ = class_signature csig in
        let header_docs =
          match toc with
          | [] -> doc
          | _ -> doc ++ (Top_level_markup.render_toc toc)
        in
        let subtree = Tree.make ~header_docs ?theme_uri expansion [] in
        Tree.leave ();
        O.a ~a:[ a_href ~kind:`Class name ] [O.txt name], [subtree]
    in
    let class_def_content =
      let open Lang.Signature in
      let keyword' =
        match recursive with
        | Ordinary | Nonrec | Rec -> "class"
        | And -> "and"
      in
      keyword keyword' ++
      O.txt " " ++
      virtual_ ++
      params ++
      O.txt " " ++
      cname ++
      O.txt Syntax.Type.annotation_separator ++
      cd
    in
    let region = [O.code class_def_content] in
    region, t.doc, [], subtree


  and class_type ?theme_uri recursive (t : Odoc_model.Lang.ClassType.t) =
    let name = Paths.Identifier.name t.id in
    let params = format_params ~delim:(`brackets) t.params in
    let virtual_ =
      if t.virtual_ then [keyword "virtual"; O.txt " "] else [] in
    let expr = class_type_expr t.expr in
    let cname, subtree =
      match t.expansion with
      | None -> O.txt name, []
      | Some csig ->
        Tree.enter ~kind:(`Cty) name;
        let doc = Comment.to_html t.doc in
        let expansion, _, _ = class_signature csig in
        let subtree = Tree.make ~header_docs:doc ?theme_uri expansion [] in
        Tree.leave ();
        O.a ~a:[ a_href ~kind:`Cty name ] [O.txt name], [subtree]
    in
    let ctyp =
      let open Lang.Signature in
      let keyword' =
        match recursive with
        | Ordinary | Nonrec | Rec ->
          [keyword "class"; O.txt " "; keyword "type"]
        | And -> [keyword "and"]
      in
      keyword' ++
      [O.txt " "] ++
      virtual_ ++
      params ++
      O.txt " " ++
      cname ++
      O.txt " = " ++
      expr
    in
    let region = [O.code ctyp] in
    region, t.doc, [], subtree
end
open Class



module Module :
sig
  val signature
    : ?heading_level_shift:Top_level_markup.heading_level_shift
    -> ?theme_uri:Tree.uri
    -> Lang.Signature.t
    -> (Html_types.div_content O.elt) list * toc * Tree.t list
end =
struct
  let signature_item_to_id : Lang.Signature.item -> _ = function
    | Type (_, {id; _}) -> path_to_id (id :> Paths.Identifier.t)
    | TypeSubstitution {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | Exception {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | Value {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | External {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | Module (_, {id; _}) -> path_to_id (id :> Paths.Identifier.t)
    | ModuleType {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | ModuleSubstitution {id; _} -> path_to_id (id :> Paths.Identifier.t)
    | Class (_, {id; _}) -> path_to_id (id :> Paths.Identifier.t)
    | ClassType (_, {id; _}) -> path_to_id (id :> Paths.Identifier.t)
    | TypExt _
    | Include _
    | Comment _ -> None

  let signature_item_to_spec : Lang.Signature.item -> _ = function
    | Type _ -> Some "type"
    | TypeSubstitution _ -> Some "type-subst"
    | Exception _ -> Some "exception"
    | Value _ -> Some "value"
    | External _ -> Some "external"
    | Module _ -> Some "module"
    | ModuleType _ -> Some "module-type"
    | ModuleSubstitution _ -> Some "module-substitution"
    | Class _ -> Some "class"
    | ClassType _ -> Some "class-type"
    | TypExt _ -> Some "extension"
    | Include _
    | Comment _ -> None

  let tag_signature_item : Lang.Signature.item -> _ = fun item ->
    match item with
    | Type _ -> `Leaf_item (`Type, item)
    | TypeSubstitution _ -> `Leaf_item (`TypeSubstitution, item)
    | TypExt _ -> `Leaf_item (`Extension, item)
    | Exception _ -> `Leaf_item (`Exception, item)
    | Value _ -> `Leaf_item (`Value, item)
    | External _ -> `Leaf_item (`External, item)
    | ModuleSubstitution _ -> `Leaf_item (`ModuleSubstitution, item)

    | Module _
    | ModuleType _
    | Include _
    | Class _
    | ClassType _ -> `Nested_article item

    | Comment comment -> `Comment comment

  let rec render_leaf_signature_item : Lang.Signature.item -> _ = function
    | Type (r, t) -> type_decl (r, t)
    | TypeSubstitution t -> type_decl ~is_substitution:true (Ordinary, t)
    | TypExt e -> extension e
    | Exception e -> exn e
    | Value v -> value v
    | External e -> external_ e
    | ModuleSubstitution m -> module_substitution m
    | _ -> assert false

  and render_nested_signature_or_class
    : ?theme_uri:Tree.uri -> Top_level_markup.heading_level_shift ->
      Lang.Signature.item -> _ =
    fun ?theme_uri heading_level item ->
    match item with
    | Module (recursive, m) -> module_ ?theme_uri recursive m
    | ModuleType m -> module_type ?theme_uri m
    | Class (recursive, c) -> class_ ?theme_uri recursive c
    | ClassType (recursive, c) -> class_type ?theme_uri recursive c
    | Include m -> include_ heading_level ?theme_uri m
    | _ -> assert false

  and signature ?heading_level_shift ?theme_uri s =
    let tagged_items = List.map tag_signature_item s in
    Top_level_markup.lay_out
      heading_level_shift
      ~item_to_id:signature_item_to_id
      ~item_to_spec:signature_item_to_spec
      ~render_leaf_item:render_leaf_signature_item
      ~render_nested_article:(render_nested_signature_or_class ?theme_uri)
      tagged_items

  and functor_argument
    : 'row. ?theme_uri:Tree.uri -> Odoc_model.Lang.FunctorParameter.parameter
    -> Html_types.div_content O.elt list * Tree.t list
  = fun ?theme_uri arg ->
    let open Odoc_model.Lang.FunctorParameter in
    let name = Paths.Identifier.name arg.id in
    let nb = functor_arg_pos arg in
    let link_name = Printf.sprintf "%d-%s" nb name in
    let def_div, subtree =
      match arg.expansion with
      | None ->
        (
          O.txt (Paths.Identifier.name arg.id) ++
          O.txt Syntax.Type.annotation_separator ++
          mty (arg.id :> Paths.Identifier.Signature.t) arg.expr
        ), []
      | Some expansion ->
        let expansion =
          match expansion with
          | AlreadyASig ->
            begin match arg.expr with
            | Signature sg -> Odoc_model.Lang.Module.Signature sg
            | _ -> assert false
            end
          | e -> e
        in
        Tree.enter ~kind:(`Arg) link_name;
        let (doc, toc, subpages) = module_expansion ?theme_uri expansion in
        let header_docs = Top_level_markup.render_toc toc in
        let subtree = Tree.make ~header_docs ?theme_uri doc subpages in
        Tree.leave ();
        (
          O.a ~a:[ a_href ~kind:`Arg link_name ] [O.txt name] ++
          O.txt Syntax.Type.annotation_separator ++
          mty (arg.id :> Paths.Identifier.Signature.t) arg.expr
        ), [subtree]
    in
    let region = [O.code def_div] in
    region, subtree

  and module_expansion
    : ?theme_uri:Tree.uri -> Odoc_model.Lang.Module.expansion
    -> Html_types.div_content_fun O.elt list * toc * Tree.t list
  = fun ?theme_uri t ->
    match t with
    | AlreadyASig -> assert false
    | Signature sg ->
      let expansion, toc, subpages = signature ?theme_uri sg in
      expansion, toc, subpages
    | Functor (args, sg) ->
      let sig_html, toc, subpages = signature ?theme_uri sg in
      let params, params_subpages =
        List.fold_left (fun (args, subpages as acc) arg ->
          match arg with
          | Odoc_model.Lang.FunctorParameter.Unit -> acc
          | Named arg ->
            let arg, arg_subpages = functor_argument ?theme_uri arg in
            let arg = O.li arg in
            (args ++ [arg], subpages ++ arg_subpages)
        )
        ([], []) args
      in
      let html =
        O.h3 ~a:[ O.a_class ["heading"] ] [ O.txt "Parameters" ] ++
        O.ul (List.map O.Unsafe.coerce_elt params) ++
        O.h3 ~a:[ O.a_class ["heading"] ] [ O.txt "Signature" ] ++
        sig_html
      in
      html, toc, params_subpages ++ subpages

  and module_
      : ?theme_uri:Tree.uri -> Odoc_model.Lang.Signature.recursive ->
        Odoc_model.Lang.Module.t ->
          rendered_item * Odoc_model.Comment.docs * toc * Tree.t list
      = fun ?theme_uri recursive t ->
    let modname = Paths.Identifier.name t.id in
    let md =
      module_decl (t.id :> Paths.Identifier.Signature.t)
        (match t.display_type with
        | None -> t.type_
        | Some t -> t)
    in
    let modname, subtree =
      match t.expansion with
      | None -> O.txt modname, []
      | Some expansion ->
        let expansion =
          match expansion with
          | AlreadyASig ->
            begin match t.type_ with
            | ModuleType (Odoc_model.Lang.ModuleType.Signature sg) ->
              Odoc_model.Lang.Module.Signature sg
            | _ -> assert false
            end
          | e -> e
        in
        Tree.enter ~kind:(`Mod) modname;
        let doc = Comment.to_html t.doc in
        let expansion, toc, subpages = module_expansion ?theme_uri expansion in
        let header_docs =
          match toc with
          | [] -> doc
          | _ -> doc ++ (Top_level_markup.render_toc toc)
        in
        let subtree = Tree.make ~header_docs ?theme_uri expansion subpages in
        Tree.leave ();
        O.a ~a:[ a_href ~kind:`Mod modname ] [O.txt modname], [subtree]
    in
    let md_def_content =
      let keyword' =
        match recursive with
        | Ordinary | Nonrec -> [keyword "module"]
        | Rec -> [keyword "module"; O.txt " "; keyword "rec"]
        | And -> [keyword "and"]
      in

      keyword' ++ O.txt " " ++ modname ++ md ++
      (if Syntax.Mod.close_tag_semicolon then [O.txt ";"] else []) in
    let region = [O.code md_def_content] in
    region, t.doc, [], subtree

  and module_decl (base : Paths.Identifier.Signature.t) md =
    begin match md with
    | Alias _ -> O.txt " = "
    | ModuleType _ -> O.txt Syntax.Type.annotation_separator
    end ++
    module_decl' base md

  and extract_path_from_mt ~(default: Paths.Identifier.Signature.t) =
    let open Odoc_model.Lang.ModuleType in
    function
    | Path (`Resolved r) ->
      (Paths.Path.Resolved.ModuleType.identifier r :> Paths.Identifier.Signature.t)
    | With (mt, _) -> extract_path_from_mt ~default mt
    | TypeOf (Odoc_model.Lang.Module.Alias (`Resolved r)) ->
      (Paths.Path.Resolved.Module.identifier r :> Paths.Identifier.Signature.t)
    | TypeOf (Odoc_model.Lang.Module.ModuleType mt) ->
      extract_path_from_mt ~default mt
    | _ -> default

  and module_decl'
    : Paths.Identifier.Signature.t -> Odoc_model.Lang.Module.decl -> text =
    fun base -> function
    | Alias mod_path ->
      Tree.Relative_link.of_path ~stop_before:true (mod_path :> Paths.Path.t)
    | ModuleType mt -> mty (extract_path_from_mt ~default:base mt) mt

  and module_type ?theme_uri (t : Odoc_model.Lang.ModuleType.t) =
    let modname = Paths.Identifier.name t.id in
    let mty =
      match t.expr with
      | None -> []
      | Some expr ->
        O.txt " = " ++ mty (t.id :> Paths.Identifier.Signature.t) expr
    in
    let modname, subtree =
      match t.expansion with
      | None -> O.txt modname, []
      | Some expansion ->
        let expansion =
          match expansion with
          | AlreadyASig ->
            begin match t.expr with
            | Some (Signature sg) -> Odoc_model.Lang.Module.Signature sg
            | _ -> assert false
            end
          | e -> e
        in
        Tree.enter ~kind:(`Mty) modname;
        let doc = Comment.to_html t.doc in
        let expansion, toc, subpages = module_expansion ?theme_uri expansion in
        let header_docs =
          match toc with
          | [] -> doc
          | _ -> doc ++ (Top_level_markup.render_toc toc)
        in
        let subtree = Tree.make ~header_docs ?theme_uri expansion subpages in
        Tree.leave ();
        O.a ~a:[ a_href ~kind:`Mty modname ] [O.txt modname], [subtree]
    in
    let mty_def =
      (
        keyword "module" ++
        O.txt " " ++
        keyword "type" ++
        O.txt " " ++
        modname ++
        mty
        ++ (if Syntax.Mod.close_tag_semicolon then [O.txt ";"] else [])
      )
    in
    let region = [O.code mty_def] in
    region, t.doc, [], subtree

  and mty
    : Paths.Identifier.Signature.t -> Odoc_model.Lang.ModuleType.expr -> text
  = fun base -> function
    | Path mty_path ->
      Tree.Relative_link.of_path ~stop_before:true (mty_path :> Paths.Path.t)
    | Signature _ ->
      [
        Syntax.Mod.open_tag;
        O.txt " ... ";
        Syntax.Mod.close_tag;
      ]
    | Functor (Unit, expr) ->
      (if Syntax.Mod.functor_keyword then [keyword "functor"] else []) ++
      O.txt " () " ++
      mty base expr
    | Functor (Named arg, expr) ->
      let name =
        let open Odoc_model.Lang.FunctorParameter in
        let to_print = O.txt @@ Paths.Identifier.name arg.id in
        match
          Tree.Relative_link.Id.href
            ~stop_before:(arg.expansion = None) (arg.id :> Paths.Identifier.t)
        with
        | exception _ -> to_print
        | href -> O.a ~a:[ O.a_href href ] [ to_print ]
      in
      (if Syntax.Mod.functor_keyword then [keyword "functor"] else []) ++
      O.txt " (" ++ name ++ O.txt Syntax.Type.annotation_separator ++
      mty base arg.expr ++
      [O.txt ")"; O.txt " "] ++ Syntax.Type.arrow ++ O.txt " " ++
      mty base expr
    | With (expr, substitutions) ->
      mty base expr ++
      O.txt " " ++
      keyword "with" ++
      O.txt " " ++
      O.list
        ~sep:[O.txt " "; keyword "and"; O.txt " "]
        ~f:(substitution base)
        substitutions
    | TypeOf md ->
      keyword "module" ++
      O.txt " " ++
      keyword "type" ++
      O.txt " " ++
      keyword "of" ++
      O.txt " " ++
      module_decl' base md

  and substitution
    : Paths.Identifier.Signature.t -> Odoc_model.Lang.ModuleType.substitution
    -> text
  = fun base -> function
    | ModuleEq (frag_mod, md) ->
      keyword "module" ++
      O.txt " " ++
      Tree.Relative_link.of_fragment ~base (frag_mod :> Paths.Fragment.t)
      ++ O.txt " = " ++
      module_decl' base md
    | TypeEq (frag_typ, td) ->
      keyword "type" ++
      O.txt " " ++
      (Syntax.Type.handle_substitution_params
        (Tree.Relative_link.of_fragment
          ~base (frag_typ :> Paths.Fragment.t))
        [format_params td.Lang.TypeDecl.Equation.params]
      ) ++
      fst (format_manifest td) ++
      format_constraints td.Odoc_model.Lang.TypeDecl.Equation.constraints
    | ModuleSubst (frag_mod, mod_path) ->
      keyword "module" ++
      O.txt " " ++
      Tree.Relative_link.of_fragment
        ~base (frag_mod :> Paths.Fragment.t) ++
      O.txt " := " ++
      Tree.Relative_link.of_path ~stop_before:true (mod_path :> Paths.Path.t)
    | TypeSubst (frag_typ, td) ->
      keyword "type" ++
      O.txt " " ++
      (Syntax.Type.handle_substitution_params
        (Tree.Relative_link.of_fragment
          ~base (frag_typ :> Paths.Fragment.t))
        [format_params td.Lang.TypeDecl.Equation.params]
      ) ++
      O.txt " := " ++
      match td.Lang.TypeDecl.Equation.manifest with
      | None -> assert false (* cf loader/cmti *)
      | Some te ->
        type_expr te

  and include_ heading_level_shift ?theme_uri (t : Odoc_model.Lang.Include.t) =
    let docs = Comment.to_html t.doc in
    let docs = (docs :> (Html_types.div_content O.elt) list) in
    let should_be_inlined =
      let is_inline_tag element =
        element.Odoc_model.Location_.value = `Tag `Inline in
      List.exists is_inline_tag t.doc
    in
    let included_html, toc, tree =
      let heading_level_shift =
        if should_be_inlined then
          Some heading_level_shift
        else
          None
      in
      signature ?heading_level_shift ?theme_uri t.expansion.content
    in
    let should_be_open =
      let is_open_tag element = element.Odoc_model.Location_.value = `Tag `Open in
      let is_closed_tag element =
        element.Odoc_model.Location_.value = `Tag `Closed in
      if List.exists is_open_tag t.doc then
        true
      else
        !Tree.open_details && not (List.exists is_closed_tag t.doc)
    in
    let incl, toc =
      if should_be_inlined then
        included_html, toc
      else
        let incl =
          O.code (
            keyword "include" ++
            O.txt " " ++
            module_decl' t.parent t.decl
            ++
            (if Syntax.Mod.include_semicolon then [keyword ";"] else [])
          )
        in
        (* FIXME: I'd like to add an anchor here, but I don't know what id to
           give it... *)
        [
          O.details ~a:(if should_be_open then [O.a_open ()] else [])
            (O.summary [O.span ~a:[O.a_class ["def"]] [incl]])
            included_html
        ], []
    in
    [
      O.div ~a:[O.a_class ["spec"; "include"]]
        [O.div ~a:[O.a_class ["doc"]]
          (docs ++ incl)]
    ],
    [],
    toc,
    tree
end
open Module



module Page :
sig
  val compilation_unit : ?theme_uri:Tree.uri -> Lang.Compilation_unit.t -> Tree.t
  val page : ?theme_uri:Tree.uri -> Lang.Page.t -> Tree.t
end =
struct
  let pack
    : Odoc_model.Lang.Compilation_unit.Packed.t ->
        Html_types.div_content O.elt list
  = fun t ->
    let open Odoc_model.Lang in
    t
    |> List.map begin fun x ->
      let modname = Paths.Identifier.name x.Compilation_unit.Packed.id in
      let md_def =
        keyword "module" ++
        O.txt " " ++
        O.txt modname ++
        O.txt " = " ++
        Tree.Relative_link.of_path ~stop_before:false (x.path :> Paths.Path.t)
      in
      [O.code md_def]
    end
    |> List.flatten
    |> fun definitions ->
      [O.article definitions]



  let compilation_unit ?theme_uri (t : Odoc_model.Lang.Compilation_unit.t) : Tree.t =
    let package =
      match t.id with
      | `Root (a, _) -> a.package
      | _ -> assert false
    in
    Tree.enter package;
    Tree.enter (Paths.Identifier.name t.id);
    let header_docs = Comment.to_html t.doc in
    let header_docs, html, subtree =
      match t.content with
      | Module sign ->
        let html, toc, subpages = signature ?theme_uri sign in
        let header_docs =
          match toc with
          | [] -> header_docs
          | _ -> header_docs ++ (Top_level_markup.render_toc toc)
        in
        header_docs, html, subpages
      | Pack packed ->
        header_docs, pack packed, []
    in
    Tree.make ~header_docs ?theme_uri html subtree



  let page ?theme_uri (t : Odoc_model.Lang.Page.t) : Tree.t =
    let package, name =
      match t.name with
      | `Page (a, name) -> a.package, name
    in
    Tree.enter package;
    Tree.enter ~kind:`Page (Odoc_model.Names.PageName.to_string name);
    let html, header_docs, toc = Top_level_markup.lay_out_page t.content in
    let html = (html :> (Html_types.div_content O.elt) list) in
    let header_docs =
      match toc with
      | [] -> header_docs
      | _ -> header_docs ++ (Top_level_markup.render_toc toc)
    in
    Tree.make ~header_docs ?theme_uri html []
end
include Page
end
