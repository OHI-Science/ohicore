\name{AggregateLayers}
\alias{AggregateLayers}
\alias{aggregate_by_country}
\alias{aggregate_by_country_weighted}
\alias{aggregate_by_country_year}
\title{Aggregate Layers}
\usage{
aggregate_by_country(df, col.value = "value", col.country = "country_id",
  lyrs.dat.csv = layers_data.csv)
}
\arguments{
  \item{df}{Input data frame.}

  \item{col.value}{Column in data frame containing the
  value to be aggregated.}

  \item{col.country}{Column in data frame containing the
  country_id.}

  \item{col.weight}{Column in data frame containing the
  weight.}
}
\value{
These functions aggregate to region by either country,
country and year, or just a weight. These are mostly only
used for aggregating a goal's status or trend calculations
to region.
}
\description{
Aggregate by country, country and year, or some other
weight.
}
\examples{
\dontrun{
    aggregate_by_country(df, col.value='value', col.country='country_id')
    aggregate_by_country_weighted(df, w, col.value='value', col.country='country_id', col.weight='weight')
    aggregate_by_country_year(df, col.value='value', col.country='country_id')
  }
}
\keyword{layers_navigation}

