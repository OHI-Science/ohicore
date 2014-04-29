\name{ReadWriteScenario}
\alias{ReadScenario}
\alias{ReadWriteScenario}
\alias{WriteScenario}
\title{Read or Write Scenario}
\usage{
WriteScenario(scenario = list(conf = ohicore::conf.Global2013.www2013, layers
  = ohicore::layers.Global2013.www2013, scores =
  ohicore::scores.Global2013.www2013, spatial =
  system.file("extdata/spatial.www2013", package = "ohicore"), dir =
  path.expand("~/myohi/scenario.Global2013.www2013")))
}
\arguments{
  \item{scenario}{list of (conf, layers, scores, spatial,
  dir)}

  \item{scenario.R}{code to source and set scenario}
}
\value{
Returns scenario
}
\description{
Read or write all the necessary elements, ie "scenario", of
the Ocean Health Index.
}
\seealso{
Conf, Layers, scores
}
\keyword{ohi}

