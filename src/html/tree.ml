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


type syntax = OCaml | Reason

type kind = [ `Arg | `Mod | `Mty | `Class | `Cty | `Page ]

let string_of_syntax = function
  | OCaml -> "ml"
  | Reason -> "re"

type uri =
  | Absolute of string
  | Relative of string

type t = {
  name : string;
  content : [ `Html ] Html.elt;
  children : t list
}

let path = Stack.create ()

let stack_to_list s =
  let acc = ref [] in
  Stack.iter (fun x -> acc := x :: !acc) s;
  !acc

let enter ?(page=false) name = Stack.push (name, page) path
let leave () = ignore @@ Stack.pop path

module Relative_link = struct
  let semantic_uris = ref false

  module Id : sig
    val href : xref_base_uri:string option -> Odoc_document.Url.t -> string
  end = struct    
    let rec drop_shared_prefix l1 l2 =
      match l1, l2 with
      | l1 :: l1s, l2 :: l2s when l1 = l2 ->
        drop_shared_prefix l1s l2s
      | _, _ -> l1, l2

    let href ~xref_base_uri { Url.Anchor. page; anchor; kind } =
      let leaf = if !semantic_uris || kind = "page" then [] else ["index.html"] in
      let target = Url.Path.as_list page @ leaf in
      match xref_base_uri with
      (* If xref_base_uri is defined, do not perform relative URI resolution. *)
      | Some xref_base_uri ->
        let page = xref_base_uri ^ String.concat "/" target in
        begin match anchor with
        | "" -> page
        | anchor -> page ^ "#" ^ anchor
        end
      | None ->
        let current_loc =
          let path =
            let _, is_page = Stack.top path in
            if is_page then
              (* Sadness. *)
              let s = Stack.copy path in
              ignore (Stack.pop s);
              s
            else path
          in
          List.map fst (stack_to_list path)
        in
        let current_from_common_ancestor, target_from_common_ancestor =
          drop_shared_prefix current_loc target
        in
        let relative_target =
          List.map (fun _ -> "..") current_from_common_ancestor
          @ target_from_common_ancestor
        in
        let page = String.concat "/" relative_target in
        begin match anchor with
        | "" -> page
        | anchor -> page ^ "#" ^ anchor
        end
  end

  (* let of_path ~stop_before p =
   *   Of_path.to_html ~stop_before p
   * 
   * let of_fragment ~base frag =
   *   Of_fragment.to_html ~stop_before:false base frag *)

  (* let to_sub_element ~kind name =
   *   (\* FIXME: Reuse [Url]. *\)
   *   let prefix =
   *     match kind with
   *     | `Mod   -> ""
   *     | `Mty   -> "module-type-"
   *     | `Arg   -> "argument-"
   *     | `Class -> "class-"
   *     | `Cty   -> "class-type-"
   *     | `Page  -> assert false
   *   in
   *   Html.a_href (prefix ^ name ^ (if !semantic_uris then "" else "/index.html")) *)
end

(* let render_fragment = Relative_link.Of_fragment.render_raw *)

let page_creator ?(is_page=false) ?(theme_uri = Relative "./") ~path name header_docs content =
  let rec add_dotdot ~n acc =
    if n <= 0 then
      acc
    else
      add_dotdot ~n:(n - 1) ("../" ^ acc)
  in
  let resolve_relative_uri uri =
    (* Remove the first "dot segment". *)
    let uri =
      if String.length uri >= 2 && String.sub uri 0 2 = "./" then
        String.sub uri 2 (String.length uri - 2)
      else uri
    in
    (* How deep is this page? *)
    let n =
      List.length path - (
        (* This is just horrible. *)
        if is_page then 1 else 0)
    in
    add_dotdot uri ~n
  in

  let head : Html_types.head Html.elt =
    let title_string = Printf.sprintf "%s (%s)" name (String.concat "." path) in

    let theme_uri =
      match theme_uri with
      | Absolute uri -> uri
      | Relative uri -> resolve_relative_uri uri
    in

    let support_files_uri = resolve_relative_uri "./" in

    let odoc_css_uri = theme_uri ^ "odoc.css" in
    let highlight_js_uri = support_files_uri ^ "highlight.pack.js" in

    Html.head (Html.title (Html.txt title_string)) [
      Html.link ~rel:[`Stylesheet] ~href:odoc_css_uri () ;
      Html.meta ~a:[ Html.a_charset "utf-8" ] () ;
      Html.meta ~a:[ Html.a_name "generator";
                     Html.a_content "odoc %%VERSION%%" ] ();
      Html.meta ~a:[ Html.a_name "viewport";
                  Html.a_content "width=device-width,initial-scale=1.0"; ] ();
      Html.script ~a:[Html.a_src highlight_js_uri] (Html.txt "");
      Html.script (Html.txt "hljs.initHighlightingOnLoad();");
    ]
  in

  let wrapped_content : (Html_types.div_content Html.elt) list =
    (* let title_prefix =
     *   match is_page with
     *   | None
     *   | Some `Mod -> Some "Module"
     *   | Some `Arg -> Some "Parameter"
     *   | Some `Mty -> Some "Module type"
     *   | Some `Cty -> Some "Class type"
     *   | Some `Class -> Some "Class"
     *   | Some  * `Page -> None
    in *)

    (* let header_docs =
     *   if is_page then
     *     header_docs
     *   else
     *     let title_heading =
     *       Html.h1 [
     *         Html.code [
     *           (\* (\\* Shorten path to at most 2 levels *\\)
     *            * match List.tl path |> List.rev with
     *            * | y :: x :: _ -> Html.txt @@ x ^ "." ^ y
     *            * | x :: _ -> Html.txt x
     *            * | _ -> Html.txt "" (\\* error *\\) *\)
     *           Html.txt name
     *         ]
     *       ]
     *     in
     *     title_heading::header_docs
     * in *)

    let header_content =
      let dot = if !Relative_link.semantic_uris then "" else "index.html" in
      let dotdot = add_dotdot ~n:1 dot in
      let up_href = if is_page && name <> "index" then dot else dotdot in
      let has_parent = List.length path > 1 in
      if has_parent then
        let nav =
          Html.nav @@ [
            Html.a ~a:[Html.a_href up_href] [
              Html.txt "Up"
            ];
            Html.txt " – "
          ] @
            (* Create breadcrumbs *)
            let space = Html.txt " " in
            let breadcrumb_spec =
              if is_page
              then (fun n x -> n, dot, x)
              else (fun n x -> n, add_dotdot ~n dot, x)
            in
            let rev_path = if is_page && name = "index"
              then List.tl (List.rev path)
              else List.rev path
            in
            rev_path |>
            List.mapi breadcrumb_spec |>
            List.rev |>
            Utils.list_concat_map ?sep:(Some([space; Html.entity "#x00BB"; space]))
              ~f:(fun (n, addr, lbl) ->
                if n > 0 then
                  [[Html.a ~a:[Html.a_href addr] [Html.txt lbl]]]
                else
                  [[Html.txt lbl]]
                ) |>
            List.flatten
        in
        nav::header_docs
      else
        header_docs
    in

    let header = Html.header header_content in

    [Html.div ~a:[Html.a_class ["content"]] (header::content)]
  in

  let html : [ `Html ] Html.elt = Html.html head (Html.body wrapped_content) in

  html

let make ?(header_docs = []) ?theme_uri title content children =
  assert (not (Stack.is_empty path));
  let name, is_page = Stack.top path in
  let path    = List.map fst (stack_to_list path) in
  let content = page_creator ~is_page ?theme_uri ~path title header_docs content in
  { name; content; children }

let traverse ~f t =
  let rec aux parents node =
    f ~parents node.name node.content;
    List.iter (aux (node.name :: parents)) node.children
  in
  aux [] t

let open_details = ref true
