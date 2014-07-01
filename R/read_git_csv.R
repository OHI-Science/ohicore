#' Read CSV from local Git repository
#' 
#' Read CSV from local Git repository.
#' 
#' @param repo path to local repository
#' @param hex SHA hex of commit
#' @param path to csv file with the repository as root
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
  # TODO: get head
  library(git2r) # need latest:  
  
  # get tree of commit
  o = tree(lookup(repository(repo), hex)) # when(commit_old); show(commit_old); summary(commit_old)
  
  # traverse tree, if in subdirectories
  if (dirname(path) != '.'){
    for (dir in str_split(dirname(path), '/')[[1]]){
      o = o[dir]
    }
  }
  
  # write and read csv content
  csv = tempfile(tools::file_path_sans_ext(basename(path)), fileext='csv')
  cat(content(o[basename(path)]), file=csv, sep='\n')
  d = read.csv(csv, ...)
  unlink(csv)
  return(d)
}
