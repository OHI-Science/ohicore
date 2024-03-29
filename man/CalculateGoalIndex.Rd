% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalculateGoalIndex.R
\name{CalculateGoalIndex}
\alias{CalculateGoalIndex}
\title{Calculate Goal Index}
\usage{
CalculateGoalIndex(
  id,
  status,
  trend,
  resilience,
  pressure,
  DISCOUNT = 1,
  BETA = 0.67,
  default_trend = 0,
  xlim = c(0, 1)
)
}
\arguments{
\item{id}{is the subregion identifier}

\item{status}{(x) score}

\item{trend}{(t) score for 5 year outloook}

\item{resilience}{(r) score}

\item{pressure}{(p) score

Constants:}

\item{DISCOUNT}{is the discount multiplier (i.e., df = 1 - rate)}

\item{BETA}{is the trend dampening multiplier used in likely future status calculation}

\item{default_trend}{The default trend value (0) if region has NA.}
}
\value{
Returns a data.frame with the input data, a likely future
status and OHI score, containing columns: status (x), trend (t), resilience (r), 
pressure (p), future status (xF) and goal score (score).
}
\description{
Goal-level computation function to goal score ("component indicators for
public goals") based on status, trend, resilience, pressure
}
\details{
Parameters:
}
\examples{

\dontrun{
## run a model with 50 regions using random data,
## using 5 year 1-percent discount rate and beta = 0.67
require(ohi)
d <- ohi.model.goal(id = 1:50, 
                    status = runif(50, 0, 1), 
                    trend = runif(50, -1, 1), 
                    resilience = runif(50, 0, 1), 
                    pressure = runif(50, 0, 1), 
                    DISCOUNT = (1 + 0.01)^-5,
                    BETA = 0.67,
                    default_trend = 0.0) 
## view model output
names(d)
d[,c('id','score','xF')]
}

}
\keyword{ohi}
