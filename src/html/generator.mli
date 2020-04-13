val render :
  ?theme_uri:Tree.uri -> string -> Odoc_document.Types.Page.t -> Tree.t

val doc :
  Odoc_document.Types.Block.t ->
  Html_types.flow5 Tyxml.Html.elt list
