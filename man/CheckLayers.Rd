\name{CheckLayers}
\alias{CheckLayers}
\title{Check Layers}
\usage{
  CheckLayers(layers.csv, layers.dir, flds_id, verbose = T,
    msg.indent = "  ")
}
\arguments{
  \item{layers.csv}{full path to the layers.csv file.}

  \item{layers.dir}{full path to the directory containing
  the layers files.}

  \item{flds_id}{character vector of unique identifiers,
  typically spatial, eg c('region_id', 'country_id',
  'saup_id'), described in your
  \code{\link{Conf}$layers_id_fields}.}

  \item{if}{True (default), extra diagnostics are output}
}
\value{
  The CheckLayers() function iterates through all the
  layers in layers.csv and updates the following field
  names: \itemize{ \item{\emph{fld_id_num} - numeric unique
  identifier} \item{\emph{fld_id_chr} - character unique
  identifier} \item{\emph{fld_category} - category}
  \item{\emph{fld_year} - year} \item{\emph{fld_val_num} -
  numeric value} \item{\emph{fld_val_chr} - character
  value} }
}
\description{
  Check all the input layers as defined by layers.csv and
  update required fields
}
\examples{
\dontrun{
  CheckLayers(layers.csv, layers.dir, c('rgn_id','cntry_key','saup_id'))
}
}
\keyword{layers}

