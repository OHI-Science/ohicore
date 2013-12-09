\name{CalculateGoalIndex}
\alias{CalculateGoalIndex}
\title{Calculate Goal Index}
\usage{
  CalculateGoalIndex(id, status, trend, resilience,
    pressure, DISCOUNT = 1, BETA = 0.67, default_trend = 0)
}
\arguments{
  \item{id}{Region identifiers.}

  \item{status}{Status scores.}

  \item{trend}{Trend values for 5 year outlook.}

  \item{resilience}{Resilience scores.}

  \item{pressure}{Pressures scores.}

  \item{DISCOUNT}{Discount multiplier.}

  \item{BETA}{Multiplier used in likely future status
  calculation.}

  \item{default_trend}{The default trend value (0) if
  region has NA.}
}
\value{
  Returns a data.frame with the input data, and a likely
  future status and OHI score.
}
\description{
  The goal model function.
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

