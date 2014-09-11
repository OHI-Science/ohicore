#' Get scenarios
#' 
#' Get scenarios from Github.
#' 
#' @param github_url_suffix suffix of Github URL in the form 'user/repo'
#' @param destination_dir destination directory
#' @details The scenario files from the containing folder are downloaded and shortcuts specific to R path and OS generated.
#' @keywords ohi
#' @import httr
#' @export
get_scenarios = function(github_repo, destination_dir, scenarios){
  library(httr)
  
  # dirs
  url = sprintf('https://github.com/%s/archive/master.zip', github_repo)
  dir_dest   = destination_dir
  dir_master = sprintf('%s-master', dir_dest)
  for (dir in c(dir_dest, dir_master)){
    cat(sprintf('Deleting %s\n', dir))
    if (file.exists(dir)) unlink(dir, recursive=T, force=T)
  }
  
  # download zip
  cat(sprintf('Downloading https://github.com/%s \n  to %s', github_repo, dir_dest))
  zip = tempfile(basename(dir_dest), fileext='zip')
  writeBin(content(GET(url)), zip)
  
  # unzip, rename, clean up
  unzip(zip, exdir=dirname(dir_dest))
  file.rename(dir_master, dir_dest)
  unlink(zip)
  
  # write launch_app shortcuts specific to R install path and operating system (OS)
  for (s in scenarios){
    cat(sprintf('Writing launch_app.* shortcut to %s', file.path(dir_dest, s)))
    ohicore::write_shortcuts(file.path(dir_dest, s))
  }
}
