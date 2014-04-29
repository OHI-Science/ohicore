# iterate over commits, installing each version of ohicore and testing for existence of Conf class in loaded library

# * https://developer.github.com/v3/#rate-limiting
# * https://developer.github.com/v3/repos/commits/

library(RJSONIO)
library(RCurl)
library(devtools)

# fetch list of commits since last working Conf.R
url = 'https://api.github.com/repos/ohi-science/ohicore/commits?since=2013-12-24T05:39:37Z&per_page=100'
d = fromJSON(getURL(url, useragent='bbest'))

# initiate log
log.txt = 'inst/tests/iterate_commits_log.txt'
cat(sprintf('starting iteration %s\n====\n\n', Sys.time()), file=log.txt)

for (i in 1:length(d)){ # i=length(d) # 89

  # get commit info
  x = d[[i]]
  sha = x$sha # secure hash algorithm (SHA)
  cat(sprintf('%s: %s\n    %s\n\n', 
              x$commit$author['date'], x$sha, x$commit$message), file=log.txt, append=T)
  
  try({
    # install package based on SHA
    Sys.sleep(80) # rate limit
    install_github('ohi-science/ohicore', ref=sha, quiet=T)
    
    # load library
    if ('ohicore' %in% loadedNamespaces()) unloadNamespace('ohicore')
    library(ohicore)
    
    # test for Conf class
    cat(sprintf('Conf exists: %s\n', exists('Conf')), file=log.txt, append=T)
  })
  
  cat(sprintf('\nfinished: %s\n----\n\n', Sys.time()), file=log.txt, append=T)
}
