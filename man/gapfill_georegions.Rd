\name{gapfill_georegions}
\alias{gapfill_georegions}
\title{Gapfill using georegional means}
\usage{
gapfill_georegions(data, georegions, fld_id = intersect(names(data),
  names(georegions)), fld_weight = NULL, fld_value = setdiff(names(data),
  c(fld_id, fld_weight)))
}
\arguments{
  \item{data}{data.frame to gapfill having at least fields:
  \code{fld_id} and \code{fld_value}, and optionally
  \code{fld_weight}}

  \item{georegions}{data.frame having at least fields:
  \code{fld_id} and \code{r0}, \code{r1}, and \code{r2}
  with georegion id values}

  \item{fld_id}{common spatial id field (eg region_id or
  country_key) between \code{data} and \code{georegions}}

  \item{fld_weight}{optional weighting field in
  \code{data}}

  \item{fld_value}{value to gapfill in \code{data}}
}
\value{
Returns a data.frame of having all the \code{fld_id} from
georegions filled in the following columns: \itemize{ \item
\code{fld_id} - spatial id (eg region_id or country_key).
\item \code{fld_value} - the gapfilled value (eg score). }
The returned data.frame also has an attribute
"gapfill_georegions" which shows the calculated georegional
means and which levels were chosen: \itemize{ \item
\code{r0} - georegional id for level 0, ie global.  \item
\code{r1} - georegional id for level 1.  \item \code{r2} -
georegional id for level 2, the finest resolution of
georegions.  \item \code{id} - spatial id (eg region_id or
country_key).  \item \code{w} - weight used to apply
\code{\link{weighted.mean}}. Defaults to 1 if not supplied
as \code{fld_weight} parameter.  \item \code{v} - original
\code{fld_value} in \code{data} \item \code{r2_v} -
weighted.mean for level 2 \item \code{r1_v} - weighted.mean
for level 1 \item \code{r0_v} - weighted.mean for level 0
\item \code{r2_n} - count of input values for level 2 \item
\code{r1_n} - count of input values for level 1 \item
\code{r0_n} - count of input values for level 0 \item
\code{z_n} - count of input values for finest level
available \item \code{z_level} - finest level available
\item \code{z} - weighted.mean for finest level available }
}
\description{
Gapfill using georegional means, providing the finest
possible resolution from 3 hierarchies (r2 > r1 > r0).
}
\details{
Gapfill using georegional means, providing the finest
possible resolution from 3 hierarchies (r2 > r1 > r0).
}
\examples{
\dontrun{
## setup
require(ohi)

# gapfill
g = gapfill_georegions(data, georegions, fld_weight='w_sum') # fld_id='rgn_id', fld_weight='w_sum', fld_value='score'

# show result and table
head(g)
head(attr(g, 'gapfill_georegions'))
}
}
\keyword{ohi}

