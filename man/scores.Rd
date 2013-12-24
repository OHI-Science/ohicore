\name{scores}
\alias{scores}
\title{scores data.frame format}
\description{
  Expected data frame format for scores.
}
\details{
  The expected scores format is the following columns:
  \itemize{ \item{\emph{region_id} - unique numeric region
  identifier, reserving 0 as the region_id for the
  area-weighted average of the entire study area}
  \item{\emph{goal} - the goal code or Index}
  \item{\emph{dimension} - the dimension code, one of:
  status, trend, pressures, resilience, future, score}
  \item{\emph{score} - the numeric score: 0-100 for all
  dimensions, except trend (-1 to 1)} } To get the wide
  view (many columns, with one row per region and columns
  having combination of goal and dimension), use something
  like: \code{reshape2::dcast(.self$long, region_id ~ goal
  + dimension, value.var='score')}.
}
\keyword{data}

