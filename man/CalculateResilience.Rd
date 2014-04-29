\name{CalculateResilience}
\alias{CalculateResilience}
\title{Calculate the resilience score for each (sub)goal.}
\usage{
  CalculateResilience(layers, conf, debug = FALSE)
}
\arguments{
  \item{layers}{object \code{\link{Layers}}}

  \item{conf}{object \code{\link{Conf}}}
}
\value{
  data.frame containing columns 'region_id' and per subgoal
  resilience score
}
\description{
  Calculate the resilience score for each (sub)goal.
}

