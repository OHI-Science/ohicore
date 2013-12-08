\name{SelectLayers}
\alias{SelectLayers}
\title{Select a set of layers.}
\usage{
  SelectLayers(object, target = NULL, layers = NULL,
    cast = T, expand.time.invariant = F)
}
\arguments{
  \item{object}{instance of Layers class}

  \item{target}{specifies the target of layers to be
  selected}

  \item{layers}{specifies the layers to be selected. If
  given as a named character vector, then layers get
  renamed with new names as values, and old names as names
  per {\code{\link{plyr::rename}}} #}

  \item{expand.time.invariant}{for layers without a year
  column, populate the same value throughout all years
  where available in other layer(s) #}

  \item{cast}{{T|F} whether to cast the resulting dataset,
  or leave it melted, defaults to TRUE}
}
\value{
  data.frame with data of selected layers with the
  following fields: \itemize{ \item{\emph{layer} - layer
  name, possibly renamed} \item{\emph{layer0} - original
  layer name, if fed a named character vector to layers}
  \item{\emph{id_num} - numeric id} \item{\emph{id_chr} -
  character id} \item{\emph{id_name} - fieldname of id}
  \item{\emph{category} - category}
  \item{\emph{category_name} - fieldname of character}
  \item{\emph{year} - year} \item{\emph{val_num} - numeric
  value} \item{\emph{val_chr} - character value}
  \item{\emph{val_name} - fieldname of value, usually in
  units as specified in Layers} }
}
\description{
  Select a set of layers.
}
\details{
  If neither target or layers are specified then all layers
  are returned. If target and layers are specified, then
  the union of the two sets of layers are returned, with
  any renamed layers renamed.
}

