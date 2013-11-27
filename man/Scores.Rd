\name{Scores}
\alias{Scores}
\title{Scores reference class.}
\usage{
  Scores(...)
}
\arguments{
  \item{results.csv}{path to comma-seperated value results
  file, long style}
}
\value{
  object (non-instantiated) reference class of Layers
  containing \itemize{ \item{\emph{long} - long view (many
  rows) of score results with columns: region, goal,
  dimension, score} \item{\emph{wide} - wide view (many
  columns) with one row per region and columns having
  combination of goal and dimension} }
}
\description{
  Scores reference class.
}
\details{
  To instantiate this object, \code{Scores(results.csv)} is
  used. The \code{results.csv} is expected to have the
  following columns: \itemize{ \item{\emph{region_id} -
  unique region identifier} \item{\emph{goal} - the goal
  code} \item{\emph{dimension} - the dimension code}
  \item{\emph{score} - the numeric score: 0-100 for all
  dimensions, except trend (-1 to 1)} }
}

