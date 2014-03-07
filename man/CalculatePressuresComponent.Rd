\name{CalculatePressuresComponent}
\alias{CalculatePressuresComponent}
\title{Calculate the pressures component of each (sub)goal.}
\usage{
CalculatePressuresComponent(eco.pressures, social.pressures,
  c.name = "category", s.name = "region", gamma = 0.5)
}
\arguments{
  \item{eco.pressures}{data.frame containing columns
  'region', 'category', 'weight', and 'value'}

  \item{social.pressures}{data.frame containing columns
  'region', and 'value'}

  \item{gamma}{(optional) if not specified defaults to 0.5}
}
\value{
data.frame containing columns 'region', 'p_E', 'p_S', and
'p_x'
}
\description{
Calculate the pressures component of each (sub)goal.
}

