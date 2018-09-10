## configure_toolbox.r

## configure_toolbox.r ensures all files in your repo are properly configured.
## It must be sourced before calculating OHI scores with ohicore::CalculateAll();
## it can be sourced here or is also sourced from calculate_scores.r.

## You are encouraged to use this script when developing individual goal models. A good workflow is:
  ## 1. prepare data layers in the /prep folders (script as much as possible in R)
  ## 2. register data layers in layers.csv and save them in /layers folder
  ## 3. source configure_repo.r to ensure proper configuration
  ## 4. develop goal models in functions.r, running individual goal models line by line

## load required packages. If not installed, install from the comment
library(ohicore)   # run install_ohicore.R
library(tidyverse) # install.packages('tidyverse')
library(stringr)   # install.packages('stringr')
library(zoo)       # install.packages('zoo')

setwd("~/github/toolbox-demo/region2016")

## load scenario configuration
conf = ohicore::Conf('conf')

## check that scenario layers files in the \layers folder match layers.csv registration. Layers files are not modified.
ohicore::CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields)

## load scenario layers for ohicore to access. Layers files are not modified.
layers = ohicore::Layers('layers.csv', 'layers')

## select corresponding data year to use for pressures and resilience
scenario_years <- 2016
layers$data$scenario_year <- scenario_years
