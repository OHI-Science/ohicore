\name{Conf}
\alias{Conf}
\title{Conf reference class.}
\usage{
  Conf(...)
}
\arguments{
  \item{dir}{path to directory containing necessary files}
}
\value{
  object reference class of Config containing: \itemize{
  \item{\emph{config}} \item{\emph{functions}}
  \item{\emph{goals}} \item{\emph{pressures_matrix}}
  \item{\emph{resilience_matrix}}
  \item{\emph{resilienceweights}} }
}
\description{
  Conf reference class.
}
\details{
  To create this object, \code{Conf(dir)}. The \code{dir}
  is expected to have the following files: \itemize{
  \item{\emph{config.R}} \item{\emph{functions.R}}
  \item{\emph{goals.csv}}
  \item{\emph{pressures_matrix.csv}}
  \item{\emph{resilience_matrix.csv}}
  \item{\emph{resilienceweights.csv}} } See also
  \code{\link{Conf_write}() to write the configuration back
  to disk.}
}

