.onLoad <- function(libname, pkgname) {
  
  # ensure dplyr functions preferentially used over plyr, ie dplyr lower in search path order, ie mask plyr functions with dplyr version
  if (which(search()=='package:dplyr') > which(search()=='package:plyr')){
    suppressPackageStartupMessages({
      detach('package:dplyr')
      library(dplyr)
    })  
  }
  #   # plyr
  #   arrange   = dplyr::arrange
  #   desc      = dplyr::desc
  #   failwith  = dplyr::failwith
  #   id        = dplyr::id
  #   mutate    = dplyr::mutate
  #   summarise = dplyr::summarise
  #   summarize = dplyr::summarize
  #   # stats
  #   filter    = dplyr::filter
  #   lag       = dplyr::lag
  #   # base
  #   intersect = dplyr::intersect
  #   setdiff   = dplyr::setdiff
  #   setequal  = dplyr::setequal
  #   union     = dplyr::union
  
  # Attaching package: ‘dplyr’
  # 
  # The following objects are masked from ‘package:plyr’:
  # 
  #     arrange, desc, failwith, id, mutate, summarise, summarize
  # 
  # The following objects are masked from ‘package:stats’:
  # 
  #     filter, lag
  # 
  # The following objects are masked from ‘package:base’:
  # 
  #     intersect, setdiff, setequal, union
  

}

.onAttach <- function(libname, pkgname) {
    
  
#   # borrowed from https://github.com/hadley/dplyr/blob/bb70227deaafc63db817610b5a35368fe1b5487d/R/zzz.r
#   setHook(packageEvent("plyr", "attach"), function(...) {
#     packageStartupMessage(rule())
#     packageStartupMessage("You have loaded plyr after dplyr - this is likely ",
#                           "to cause problems.\nIf you need functions from both plyr and dplyr, ",
#                           "please load plyr first, then dplyr:\nlibrary(plyr); library(dplyr)")
#     packageStartupMessage(rule())
#   })

}