\name{CalculatePressures}
\alias{CalculatePressures}
\title{Calculate the pressures component of each (sub)goal.}
\usage{
  CalculatePressures(layers, conf, gamma, debug = F)
}
\arguments{
  \item{layers}{object \code{\link{Layers}}}

  \item{conf}{object \code{\link{Conf}}}

  \item{gamma}{(optional) if not specified defaults to 0.5}
}
\value{
  data.frame containing columns 'region_id' and per subgoal
  pressures score
}
\description{
  Calculate the pressures component of each (sub)goal.
}

