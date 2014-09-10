load_dplyr = function(env=NULL){
  suppressPackageStartupMessages(library(plyr))
  suppressPackageStartupMessages(library(dplyr))
  for (fxn in c('arrange','desc','failwith','id','mutate','summarize','summarise','filter','lag','intersect','setdiff','setequal','union')){
    if (environmentName(environment(get(fxn))) != 'dplyr'){
      #message(sprintf('load_dplyr: setting %s to dplyr', fxn))
      suppressWarnings( eval(parse(text=sprintf('%s = dplyr::%s', fxn, fxn)), envir=.GlobalEnv) )
      if (!is.null(env)){
        suppressWarnings( eval(parse(text=sprintf('%s = dplyr::%s', fxn, fxn)), envir=env) )
      }
    }
  }
}
