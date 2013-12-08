\name{TransformSpatialScheme}
\alias{TransformSpatialScheme}
\title{Transform data}
\usage{
  TransformSpatialScheme(object, data, target, origin,
    categories)
}
\arguments{
  \item{object}{instance of SpatialSchemes class}

  \item{data}{data.frame such as returned from
  'SelectLayersData' function}

  \item{target}{single spatial scheme to which data should
  be transformed}

  \item{origin}{spatial schemes from which to transform,
  can be vector}

  \item{categories}{layers for which transformation should
  be done (to be safe, for now this should be all the
  layers in param data)}
}
\value{
  data.frame transformed data
}
\description{
  Transform data
}

