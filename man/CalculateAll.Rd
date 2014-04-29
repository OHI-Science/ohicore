\name{CalculateAll}
\alias{CalculateAll}
\title{Calculate All}
\usage{
CalculateAll(conf, layers, debug = F)
}
\arguments{
  \item{conf}{of class \code{\link{Conf}}}

  \item{layers}{of class \code{\link{Layers}}}

  \item{debug}{print debug messages (default=FALSE)}
}
\value{
Returns a data.frame of scores having the following
columns: \itemize{ \item{\emph{region_id} - unique numeric
region identifier, reserving 0 as the region_id for the
area-weighted average of the entire study area}
\item{\emph{goal} - the goal code or Index}
\item{\emph{dimension} - the dimension code, one of:
status, trend, pressures, resilience, future, score}
\item{\emph{score} - the numeric score: 0-100 for all
dimensions, except trend (-1 to 1)} }
}
\description{
Calculate all scores, given layers and configuration.
}
\examples{
\dontrun{
## run a model with 50 regions using random data,
## using 5 year 1-percent discount rate and beta=0.67
require(ohi)
d <- ohi.model.goal(id=1:50,
                    status=runif(50, 0, 1),
                    trend=runif(50, -1, 1),
                    resilience=runif(50, 0, 1),
                    pressure=runif(50, 0, 1),
                    DISCOUNT = (1 + 0.01)^-5,
                    BETA = 0.67,
                    default_trend = 0.0)
## view model output
names(d)
d[,c('id','score','xF')]
}
}
\keyword{ohi}

