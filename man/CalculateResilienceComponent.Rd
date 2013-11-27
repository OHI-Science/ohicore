\name{CalculateResilienceComponent}
\alias{CalculateResilienceComponent}
\title{Calculate the Resilience component of each (sub)goal.}
\usage{
  CalculateResilienceComponent(goal.specific.regulations,
    ecological.integrity, social.integrity,
    c.name = "category", s.name = "region", gamma = 0.5)
}
\arguments{
  \item{goal.specific.regulations}{(data.frame) contains
  columns 'region', 'weight', and 'value'}

  \item{gamma}{(numeric) represents the weighting between
  ecological and social aspects of resilience, defaults to
  0.5 (equal weights)}
}
\value{
  (data.frame)
}
\description{
  Calculate the Resilience component of each (sub)goal.
}

