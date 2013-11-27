\name{Layers}
\alias{Layers}
\title{Layers reference class.}
\usage{
  Layers(...)
}
\arguments{
  \item{layers.csv}{path to comma-seperated value file with
  row of metadata per layer}

  \item{layers.dir}{path of directory containing individual
  layer files}
}
\value{
  object (non-instantiated) reference class of Layers
  containing \itemize{ \item{\emph{meta} - metadata
  data.fram of original layers.csv} \item{\emph{data} -
  named list of data frames, one per layer} }
}
\description{
  Layers reference class.
}
\details{
  To instantiate this object, \code{Layers(layers.csv,
  layers.dir)} is used. The \code{layers.csv} is expected
  to have the following columns: \itemize{
  \item{\emph{layer_id} - unique identifier (no spaces or
  special characters)} \item{\emph{target} - the pipe
  delimited list of targets (goals, Pressure or Resilience)
  to feed this data layer} \item{\emph{title} - full title
  of the variable} \item{\emph{description} detailed
  description} \item{\emph{citation} - reference for
  documentation} \item{\emph{units} - indicating units and
  required column name in the layer csv file}
  \item{\emph{filename} - the csv data file for the layer}
  } The layers.dir directory should contain all the csv
  filenames listed in the layers.csv file.
}

