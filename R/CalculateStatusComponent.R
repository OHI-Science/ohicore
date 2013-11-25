#' Compute a single subgoal.
#' 
#' @param DATA data.frame containing columns 'region', 'value', and (optionally) 'w'
#' @param fun (optional) function for calculating the subgoal value, if not specified it will default to a weighted average
#' @param w (optional) numeric vector describing the 
#' @return stuff
#' @export
#' 
CalculateStatusComponent = function (DATA, fun, trend.Years = 5,
                                     c.name = 'year', s.name = 'region') {

    pre.status <- plyr::ddply(DATA,
                              c(s.name, c.name),
                              plyr::splat(fun))
    names(pre.status) <- gsub("V1", "status", names(pre.status))
    
    status <- plyr::ddply(pre.status,
                          s.name,
                          function (rows) {
                            return (rows[rows$year == max(rows$year), ])
                          })

    trend <- plyr::ddply(pre.status,
                         s.name,
                         function (rows) {
                           plyr::desc(rows)
                           n.rows <- rows[c((nrow(rows)-trend.Years+1):
                             nrow(rows)), ]
            
                           s.lm <- lm(status ~ year, data = n.rows)

                           c('trend' = as.numeric(s.lm$coefficients['year']))
                         })
    
    
    return (plyr::join(status, trend, by=s.name))
}
