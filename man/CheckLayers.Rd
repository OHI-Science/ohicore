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
  warning messages
}
\description{
  Check all the input layers as defined by layers.csv and
  update required fields
}
\details{
  The CheckLayers() function iterates through all the
  layers in layers.csv and updates the following field
  names: \itemize{ \item{\emph{fld_id_num} - numeric unique
  identifier} \item{\emph{fld_id_chr} - character unique
  identifier} \item{\emph{fld_category} - category}
  \item{\emph{fld_year} - year} \item{\emph{fld_val_num} -
  numeric value} \item{\emph{fld_val_chr} - character
  value} } Additional diagnostic fields are updated:
  \itemize{ \item{\emph{file_exists} - input filename
  exists} \item{\emph{val_min} - minimum value, if numeric}
  \item{\emph{val_max} - maximum value, if numeric}
  \item{\emph{val_0to1} - TRUE if value ranges between 0
  and 1} \item{\emph{flds_unused} - unused fields from
  input file when guessing prescribed field names (aboves)}
  \item{\emph{flds_missing} - fields expected, as given by
  Layers units, and not found} \item{\emph{rows_duplicated}
  - given the combination of all row-identifying fields
  (and excluding value fields), the number of rows which
  are duplicates} \item{\emph{num_ids_unique} - number of
  unique ids, as provided by just the unique instances of
  the fld_id} }
}
\examples{
\dontrun{
  CheckLayers(layers.csv, layers.dir, c('rgn_id','cntry_key','saup_id'))
}
}
\keyword{layers}

