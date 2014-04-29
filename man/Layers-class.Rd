\docType{class}
\name{Layers-class}
\alias{Layers-class}
\title{Layers reference class.}
\arguments{
  \item{layers.csv}{path to comma-seperated value file with
  row of metadata per layer}

  \item{layers.dir}{path of directory containing individual
  layer files}
}
\value{
object (non-instantiated) reference class of Layers
containing \itemize{ \item{\emph{meta} - metadata data
frame of original layers.csv} \item{\emph{data} - named
list of data frames, one per layer} \item{\emph{targets} -
named list of character vector indicating a layer's
targets, goal (status, trend) or dimension (pressures,
resilience)} }
}
\description{
Layers reference class.
}
\details{
To instantiate this object, \code{Layers(layers.csv,
layers.dir)} is used. The \code{layers.csv} is expected to
have the following columns: \itemize{ \item{\emph{layer} -
unique layer identifier (no spaces or special characters)}
\item{\emph{targets} - a space delimited list of targets
(goal code, 'Pressures', 'Resilience' or 'Regions') for
which this layer is applied} \item{\emph{name} - name of
the variable} \item{\emph{description} - detailed
description} \item{\emph{units} - units of the value}
\item{\emph{citation} - reference for documentation,
typically a heading code for a supplemental document}
\item{\emph{filename} - the csv data file for the layer}
\item{\emph{fld_value} - required field in the layer csv
file containing the value, which is often best named as a
shorthand for the units without spaces or special
characters} } The layers.dir directory should contain all
the csv filenames listed in the layers.csv file.
}

