#' Trace Value from CSV through history of local Git repository
#' 
#' Trace Value from CSV through history of local Git repository
#' 
#' @param repo path to repository on local filesystem
#' @param csv path to csv file with the repository as root
#' @param subset_str subset argument to the function \code{\link{subset}} quoted as string to extract row of data from csv
#' @param select field to select from subsetted row
#' 
#' @return data.frame having columns: hex, when, message, v.
#' 
#' @details If you have trouble running this function, please make sure:
#' 1) your path resolves to a local Git repository, 
#' 2) you have the latest git2r (try \code{devtools::install_github('ropensci/git2r')}).
#'  
#' @keywords git
#' @examples
#'
#' \dontrun{
#' # trace the value for a csv from github repository
#' d = trace_git_csv_value('~/github/ohicore', 'inst/extdata/scores.Global2013.www2013.csv', "goal=='ECO' & dimension=='status' & region_id==237", 'score')
#' head(d)
#' }
#' 
#' @export
trace_git_csv_value = function(repo, csv, subset_str, select, verbose=T){

  ks = git2r::commits(git2r::repository(repo), topological=F, time=T, reverse=F)
  d = 
    data.frame(
      hex     = sapply(ks, function(x) x@hex),
      when    = sapply(ks, function(x) when(x)),
      message = sapply(ks, function(x) x@message),
      v       = NA, stringsAsFactors=F)
  
  for (i in 1:nrow(d)){ # i=1
    d_csv = read_git_csv(repo, d$hex[i], csv, stringsAsFactors=F)
    if (!is.null(d_csv)){
      d$v[i] = subset(x=d_csv, subset=eval(parse(text=subset_str)), select=select, drop=T)
    }
    if (verbose){ cat(sprintf('## %03d of %d. %s [%s] %g\n', i, nrow(d), d$when[i], str_sub(d$hex[i], 1, 8), d$v[i])) }
  }
  return(d)
}