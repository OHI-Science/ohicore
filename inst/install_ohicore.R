# See: https://gist.github.com/bbest/ed6a03258c3815a3e2ba

# remove old packages
for (p in c('ohicore','ohigui','rCharts')){  
  if (p %in% rownames(installed.packages())){
    remove.packages(p)  
  }
}

# install dependencies
for (p in c('devtools','httr')){
  if (!require(p, character.only=T)){
    install.packages(p)
    require(p, character.only=T)
  } 
}

# load libraries
library(devtools)
library(httr)

# install packages
install_github('ohi-science/rCharts')
install_github('ohi-science/ohicore')

# download ohi-global scenarios
url = 'https://github.com/bbest/ohi-global/archive/master.zip'
dir = '~/ohi-global'
zip = tempfile('ohi-global', fileext='zip')
writeBin(content(GET(url)), zip)
unzip(zip, exdir=dirname(dir))
file.rename(file.path(dirname(dir),'ohi-global-master'), dir)
unlink(zip)

# write launch_app files specific to R install path and operating system (OS)
for (subdir in list.dirs(dir, full.names=T, recursive=F)){
  ohicore::write_shortcuts(subdir)
}

# launch app with eez2013 scenario, dependant on OS
setwd('~/ohi-global/eez2013')
if (.Platform$OS.type == 'windows'){  
  shell('launch_app.bat')
} else { # presume Mac
  system('open launch_app.command')
}