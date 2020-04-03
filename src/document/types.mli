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

module rec Attr : sig

  type one = string * string
  type t = one list

end

and Tabular : sig

  type t = Block.t list list

end

and InternalLink : sig

  type resolved = Url.t * Inline.t
  type unresolved = Inline.t
  type t =
    | Resolved of resolved
    | Unresolved of Inline.t

end

and Raw_markup : sig

  type target = Odoc_model.Comment.raw_markup_target 
  and t = target * string

end

and Source : sig

  type t = token list
  and decoration = string
  and token = decoration option * Inline.t

end

and Inline : sig

  type t = one list

  and one =
    | Text of string
    | Entity of entity
    | Linebreak
    | Styled of style * t
    | Link of href * t
    | InternalLink of InternalLink.t
    | Source of Source.t
    | Verbatim of string
    | Raw_markup of Raw_markup.t
  
end

and Heading : sig

  type t = {
    label : string ;
    level : int ;
    title : Inline.t ;
  }

end

and Block : sig

  type t = one list

  and one =
    | Inline of Inline.t
    | Paragraph of Inline.t
    | List of list_type * t list
    | Description of (Inline.t * t) list
    (* | Math_blk of Math.t *)
    | Source of Source.t
    | Verbatim of string
    | Tabular of Tabular.t
    (* | Table of Wrapper.t * t *)
    (* | Picture of href * string * string option * int option *)
    (* | Figure of Wrapper.t * t *)
    | Heading of Heading.t
    (* | Title of Level.title * Inline.seq *)
    | Rule
    | Raw_markup of Raw_markup.t

  and list_type =
    | Ordered
    | Unordered
  
end
