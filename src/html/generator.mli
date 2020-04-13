val render :
  ?theme_uri:Tree.uri ->
  string -> Odoc_document.Types.Page.t -> Tree.t

val doc :
  xref_base_uri:string option ->
  Odoc_document.Types.Block.t ->
  Html_types.flow5 Tyxml.Html.elt list
