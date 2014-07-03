#' Read CSV from local Git repository
#' 
#' Read CSV from local Git repository.
#' 
#' @param repo path to local repository
#' @param hex SHA hex of commit
#' @param path to csv file with the repository as root
#' 
#' @details If you have trouble running this function, please make sure:
#' 1) your path resolves to a local Git repository, 
#' 2) the hex is valid (see history in RStudio Git or on Github) and 
#' 3) you have the latest git2r (try \code{devtools::install_github('ropensci/git2r')}).
#'  
#' @keywords git
#' @examples
#'
#' \dontrun{
#' # get csv from github repository by SHA hex of commit
#' d = read_git_csv('~/github/ohi-global', 'a81a8213', 'scores.csv')
#' head(d)
#' }
#' 
#' @export
read_git_csv = function(repo, hex, path, ...){  
  # DEBUG:  repo = '~/github/ohi-global'; hex = 'a81a82131f'; path = 'eez2013/layers/np_harvest_relative.csv'; path='scores.csv'
  # TODO: get head. see markdown_link function. head(repo)@hex
  library(git2r) # need latest: devtools::install_github('ropensci/git2r')
  
  # get tree of commit
  o = tree(lookup(repository(repo), hex)) # when(commit_old); show(commit_old); summary(commit_old) # Error during wrapup: 'match' requires vector arguments
  
  # traverse tree, if in subdirectories
  if (dirname(path) != '.'){
    for (dir in str_split(dirname(path), '/')[[1]]){
      o = o[dir]
    }
  }
  
  # write and read csv content
  csv = tempfile(tools::file_path_sans_ext(basename(path)), fileext='csv')
  #browser()
  if (is_blob(o[basename(path)])){
    
#     rda = tempfile(tools::file_path_sans_ext(basename(path)), fileext='rda')
#     cat(content(o[basename(path)], split=F), file=rda, sep='')
    
    cat(content(o[basename(path)]), file=csv, sep='\n')    
    d = read.csv(csv, ...)
    unlink(csv)
  } else {
    d = NULL
  }
  return(d)
}
