#' Compute a single subgoal.
#' 
#' @param DATA data.frame containing columns 'region', 'value', and (optionally) 'w'
#' @param fun (optional) function for calculating the subgoal value, if not specified it will default to a weighted average
#' @param w (optional) numeric vector describing the 
#' @return stuff
#' @export
#' 
subcalc.Status.Trend = function (DATA, fun, trend.Years) {
    
    pre.Status = plyr::ddply(
        DATA,
        c('region', 'year'),
        plyr::splat(fun)
    )
    names(pre.Status) = gsub("V1", "status", names(pre.Status))
    
    
    
    status = plyr::ddply(
        pre.Status,
        'region',
        function (rows) {
            return ( rows[rows$year == max(rows$year),] )
        }
    )

    
    trend = plyr::ddply(
        pre.Status,
        'region',
        function (rows) {
            plyr::desc(rows)
            n.rows = rows[c( (nrow(rows)-trend.Years+1): nrow(rows)),]
            
            s.lm = lm(status ~ year, data=n.rows)

            return ( s.lm$coefficients['year'] )
        }
    )
    
    
    
    return ( list( "status"=status, "trend"=trend ) )
}



