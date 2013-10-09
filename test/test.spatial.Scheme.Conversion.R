source('../R/spatial.Scheme.Conversion.R')



# Dense data.frame
test.1 = function() {

    fun = spatial.Scheme.Conversion(
        data.frame(
            space =    c('a','b','a','b'),
            category =  c(1,1,2,2),
            value =     c(2, 4, 6, 8)
        ),
        'region',
        list(
            space = data.frame(
                space =        c('a','b','a','b'),
                region =        c('x','x','y','y'),
                w =             c(0.25, 0.75, 0.6, 0.4)
            )
        )
    )
    
    exp = data.frame(
                value = c(3.5,7.5,2.8,6.8),
                category = c(1,2,1,2),
                region = c('x','x','y','y')
    )
    
    RUnit::checkEquals(sort(names(fun)), sort(names(exp)))
    RUnit::checkEquals(fun$value[order(fun$category)], exp$value[order(exp$category)])

}


# Sparse data.frame
test.2 = function() {

    fun = spatial.Scheme.Conversion(
        data.frame(
            space =    c('a','b','a','b'),
            category =  c(1,1,2,2),
            value =     c(2, 4, 6, 8)
        ),
        'region',
        list(
            space = data.frame(
                space =        c('a','b','a'),
                region =        c('x','x','y'),
                w =             c(0.25, 0.75, 1)
            )
        )
    )
    
    exp = data.frame(
                value = c(3.5,7.5,2,6),
                category = c(1,2,1,2),
                region = c('x','x','y','y')
    )
    
    RUnit::checkEquals(sort(names(fun)), sort(names(exp)))
    RUnit::checkEquals(fun$value[order(fun$category)], exp$value[order(exp$category)])

}




















print ( test.1() )
print ( test.2() )







