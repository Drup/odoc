type entity = string

type href = string  

type reference = Url.t


type style = [
  | `Bold
  | `Italic
  | `Emphasis
  | `Superscript
  | `Subscript
]

module rec Class : sig

  type t = string list

end = Class

(* and Tabular : sig
 * 
 *   type t = {
 *     header : line option ;
 *     lines : line List.t ;
 *   }
 * 
 *   and line = {
 *     attr : Class.t ;
 *     desc : Block.t list ;
 *   }
 *         
 * end = Tabular *)

and InternalLink : sig

  type resolved = Url.t * Inline.t
  type unresolved = Inline.t
  type t =
    | Resolved of resolved
    | Unresolved of Inline.t

end = InternalLink

and Raw_markup : sig

  type target = Odoc_model.Comment.raw_markup_target 
  and t = target * string

end = Raw_markup

and Source : sig

  type t = token list
  and decoration = string
  and token = decoration option * Inline.t

end = Source

and Inline : sig

  type t = one list

  and one = {
    attr : Class.t ;
    desc : desc ;
  }
  
  and desc =
    | Text of string
    | Entity of entity
    | Linebreak
    | Styled of style * t
    | Link of href * t
    | InternalLink of InternalLink.t
    | Source of Source.t
    | Raw_markup of Raw_markup.t

end = Inline

and Heading : sig

  type t = {
    label : string ;
    level : int ;
    title : Inline.t ;
  }

end = Heading

and Block : sig

  type t = one list

  and one = {
    attr : Class.t ;
    desc : desc ;
  }
  
  and desc =
    | Inline of Inline.t
    | Paragraph of Inline.t
    | List of list_type * t list
    | Description of (Inline.t * t) list
    | Source of Source.t
    | Verbatim of string
    | Heading of Heading.t
    | Raw_markup of Raw_markup.t
    (* | DocumentedSrc of DocumentedSrc.t *)
    (* | Math_blk of Math.t *)
    (* | Tabular of Tabular.t *)
    (* | Table of Wrapper.t * t *)
    (* | Picture of href * string * string option * int option *)
    (* | Figure of Wrapper.t * t *)
    (* | Rule *)

  and list_type =
    | Ordered
    | Unordered
  
end = Block

and DocumentedSrc : sig

  type t = line list
  and line =
    | Code of {
      attr : Class.t ;
      code : Source.t ;
    }
    | Documented of {
        attrs : Class.t ;
        anchor : string ;
        code : Inline.t ;
        doc : Block.t ;
      }
    | Nested of {
        attrs : Class.t ;
        anchor : string ;
        code : t ;
        doc : Block.t ;
      }

end = DocumentedSrc

and Nested : sig

  type status = [ `Inline | `Open | `Closed ]

  type t = {
    content : Block.t ;
    status : status ;
    items : Item.t list ;
  }
  
end = Nested


and Item : sig

  type 'a item = {
    attr : Class.t ;
    anchor : string option ;
    content : 'a ;
  }

  type declaration = DocumentedSrc.t item
  type text = Block.t item
  
  type t =
    | Text of text
    | Declarations of declaration list * Block.t option
    | Nested of Nested.t item * Block.t option
    | Section of Block.t * t list

end = Item

and Page : sig

  type t = {
    title : string ;
    header : Block.t ;
    items : Item.t list ;
    toc : Toc.t ;
    subpages : t list ;
    url : Url.t ;
  }

end = Page

and Toc : sig

  type t = one list
  
  and one = {
    anchor : string ;
    text : Inline.t ;
    children : t ;
  }

end = Toc

let inline ?(attr=[]) desc = Inline.{attr ; desc}
let block ?(attr=[]) desc = Block.{attr ; desc}
