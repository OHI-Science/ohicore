## calculate_scores.R

## This script calculates OHI scores with the `ohicore` package.
## - configure_toolbox.r ensures your files are properly configured for `ohicore`.
## - The `ohicore` function CalculateAll() calculates OHI scores.


## run the configure_toolbox.r script to check configuration
source("~/github/toolbox-demo/region2016/configure_toolbox.R")

## calculate scenario scores
scores <-  ohicore::CalculateAll(conf, layers)

## save scores as scores.csv
write.csv(scores, 'scores.csv', na='', row.names=FALSE)
