\name{ReportScores}
\alias{ReportScores}
\title{Report Scores}
\usage{
  ReportScores(scenario = list(conf = ohicore::conf.Global2013.www2013, layers = ohicore::layers.Global2013.www2013, scores = ohicore::scores.Global2013.www2013, spatial = ifelse(file.exists(system.file("extdata/spatial.www2013", package = "ohicore")), system.file("extdata/spatial.www2013", package = "ohicore"), system.file("inst/extdata/spatial.www2013", package = "ohicore"))),
    directory = path.expand("~/myohi/scenario.Global2013.www2013/reports"),
    filename = "report_Global2013_www2013.html",
    overwrite = F, global_only = F, ck_Equations = T,
    ck_Histograms = T, ck_Maps = T, ck_Paths = T,
    ck_Tables = T, ck_Flowers = T, debug = F, ...)
}
\arguments{
  \item{scenario}{list of (conf, layers, scores, spatial)}

  \item{directory}{output directory of report}

  \item{filename}{output filename of report (should be
  html, pdf implementation later)}
}
\value{
  Returns path to report
}
\description{
  Generate report on calculated scores of the Ocean Health
  Index.
}
\details{
  You'll need the
  \href{http://www.imagemagick.org}{ImageMagick} program,
  especially the \code{convert} executable.  On Windows,
  you can try installing with the following \code{if
  (!require('installr')) install.packages('installr') #
  installr::install.ImageMagick() # doesn't seem to work
}
\seealso{
  Conf, Layers, scores
}
\keyword{ohi}
\keyword{report}

