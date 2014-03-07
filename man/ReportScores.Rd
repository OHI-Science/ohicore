\name{ReportScores}
\alias{ReportScores}
\title{Report Scores}
\usage{
ReportScores(scenario = list(conf = ohicore::conf.Global2013.www2013, layers =
  ohicore::layers.Global2013.www2013, scores =
  ohicore::scores.Global2013.www2013, spatial =
  ifelse(file.exists(system.file("extdata/spatial.www2013", package =
  "ohicore")), system.file("extdata/spatial.www2013", package = "ohicore"),
  system.file("inst/extdata/spatial.www2013", package = "ohicore"))),
  directory = path.expand("~/myohi/scenario.Global2013.www2013/reports"),
  filename = "report_Global2013_www2013.html", open_html = T,
  overwrite = F, global_only = T, do_flowers = T, do_tables = T,
  do_maps = T, do_histograms = T, do_paths = T, do_equations = T,
  debug = F, ...)
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
especially the \code{convert} executable.  On Windows, you
can try installing with the following \code{if
(!require('installr')) install.packages('installr') #
installr::install.ImageMagick() # doesn't seem to work
installr::install.URL('http://www.imagemagick.org/download/binaries/ImageMagick-6.8.7-10-Q16-x86-dll.exe')}
}
\seealso{
\code{\link{Conf}}, \code{\link{Layers}} and
\code{\link{scores}}
}
\keyword{ohi}
\keyword{report}

