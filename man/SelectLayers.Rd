\name{SelectLayers}
\alias{SelectLayers}
\title{Select a set of layers.}
\usage{
  SelectLayers(object, mode = "all", cast = T,
    target = NULL, layers = NULL,
    expand.time.invariant = F,
    alternate.layer.names = NULL)
}
\arguments{
  \item{object}{instance of Layers class}

  \item{mode}{{all|target|layers} defines how to select
  layers}

  \item{target}{only needed if mode='target', specifies the
  target (from layers.navigation) which should be selected}

  \item{layers}{only needed if mode='layers', specifies the
  layers which should be selected}

  \item{alternate.layer.names}{aliases for layer names}

  \item{expand.time.invariant}{for layers without a year
  column, populate the same value throughout all years
  where available in other layer(s)}

  \item{cast}{{T|F} whether to cast the resulting dataset,
  or leave it melted, defaults to TRUE}
}
\value{
  data.frame with data from selected layers
}
\description{
  Select a set of layers.
}

