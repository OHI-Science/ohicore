# install_ohi_local.R will install the OHI Toolbox without requiring access to files on Github 

# make sure to install the latest version of R
version # display current R version; check http://cran.r-project.org/ for any updates
  
# update packages
# update.packages(ask=F)

# set working directory to folder containing downloaded zip files:
#   1. rCharts-master.zip:    contains mapping support
#   2. ohicore-master.zip:    contains all OHI Toolbox functionality
#   3. ohi-global-master.zip: contains all data from OHI global assessments
setwd('C:/Users/visitor/Downloads') # set this to local directory

#remove old packages
for (p in c('ohicore','ohigui','rCharts')){  
  if (p %in% rownames(installed.packages())){
    lib = subset(as.data.frame(installed.packages()), Package==p, LibPath, drop=T)
    remove.packages(p, lib)  
  }
}

# install dependencies
for (p in c('httr','devtools')){
  if (!require(p, character.only=T)){
    install.packages(p)
    require(p, character.only=T)
  } 
}

# install packages from downloaded .zip files
install_local('rCharts-master.zip')
install_local('ohicore-master.zip')

# extract ohi-global scenarios
dir = '~/ohi-global'
if (file.exists(dir)) unlink(dir, recursive=T, force=T)
unzip('ohi-global-master.zip', exdir=dirname(dir))

file.rename(file.path(dirname(dir),'ohi-global-master'), dir)

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
  
  
  
# --- finished ---  
  