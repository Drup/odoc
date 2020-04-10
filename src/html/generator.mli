val keyword : string -> [> Html_types.span ] Tyxml.Html.elt

val render :
  ?theme_uri:Tree.uri -> Odoc_document.Types.Page.t -> Tree.t
