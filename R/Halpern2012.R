# Summary of work
# Done (not necessarily tested)
#   FP
#       FIS
#       MAR
#   AO
#   CS
#   CP
#   LE
#       LIV
#       ECO
#   TR
#   SP
#       ICO
#       LSP
#   CW
#   BD
#       SPP
#       HAB
#   
# 
# Not Done
#   NP
# 
# 




#' Calculate Natural Products. (Needs work)
#' 
#' @param N number of products that have ever been harvested
#' @param wp proportional peak dollar value of each product relative to the total peak dollar value of all products
#' @param Hp harvest of a product relative to its buffered peak reference point
#' @param E exposure term
#' @param R risk term
#' @param Nv 1 or 2, depending on whether or not a viability term is used
#' @param Nk number of species in each k category of exploitation
#' @param w weight assigned to each k category of exploitation status
#' @return 
#' @export
#' 
Halpern2012.NP = function (N, wp, Hp, E, R, Nv, Nk, w, ...) {
    
    Sp = 
    
    
    
    
    
}































#' Calculate Fisheries subgoal of Food Provision.
#' 
#' @param dBt absolute difference between landed biomass and mMSY
#' @param mMSY multi-species maximum sustainable yield
#' @param Tc taxonomic report quiality correction factor
#' @param Bt wild-caught fishing yield
#' @return 
#' @export
#' 
Halpern2012.FP.FIS <- function (dBt, mMSY, Bt, Tc, ...) {
    mMSYr = 0.75 * mMSY
    xFIS = ( 1 - ( dBt / mMSYr ) ) * Tc
    
    return ( xFIS )
}

#' Calculate Mariculture subgoal of Food Provision.
#' 
#' @param k each mariculture species
#' @param Smk sustainability score for each species k
#' @param Ac area of coastal waters (3nm strip)
#' @param Yl yield of each species k
#' @return 
#' @export
#' 
Halpern2012.FP.MAR = function (k, Smk, Ac, Yk, ...) {

    print(k)

    Yc = ( sum ( Yk * Smk ) ) / Ac
    xMAR = log10 ( Yc + 1.0 )
    
    return ( xMAR )
}

#' Calculate Food Provision.
#' 
#' @param k each mariculture species
#' @param Smk sustainability score for each species k
#' @param Ac area of coastal waters (3nm strip)
#' @param Yl yield of each species k
#' @return 
#' @export
#' 
Halpern2012.FP = function (w, dBt, mMSY, Bt, Tc, k, Smk, Ac, Yk, ...) {
    xFIS = Halpern2012.FP.FIS(dBt, mMSY, Bt, Tc)
    xMAR = Halpern2012.FP.MAR(k, Smk, Ac, Yk)
    
    xFP = ( w * xFIS ) + ( ( 1 - w ) * xMAR )
    
    return ( xFP )
}



#' Calculate Artisanal Fishing Opportunities.
#' 
#' @param Sao 
#' @param Oao 
#' @param PPPpcGDP 
#' @return 
#' @export
#' 
Halpern2012.AO = function (Sao, Oao, PPPpcGDP, ...) {
    Du = ( 1 - PPPpcGDP ) * ( 1 - Oao )
    xAO = ( 1 - Du ) * Sao
    
    return ( xAO )
}



#' Calculate Carbon Storage
#' 
#' @param Cc current 'condition' of habitat k
#' @param Cr reference 'condition' of habitat k
#' @param A amount of area covered by habitat k
#' @return 
#' @export
#' 
Halpern2012.CS = function (Cc, Cr, A, ...) {
    At = sum(A)

    xCS = sum( (Cc / Cr) * (Ak / At))

    return (xCS)
}



#' Calculate Coastal Protection
#' 
#' @param Cc current 'condition' of habitat k
#' @param Cr reference 'condition' of habitat k
#' @param A amount of area covered by habitat k
#' @param w rank weight of habitat protective ability
#' @return 
#' @export
#' 
Halpern2012.CP = function (Cc, Cr, w, A, ...) {
    wmax = max(w)
    At = sum(A)

    xCP = sum ( (Cc / Cr) * (w / wmax) * (A / At) )
    
    return (xCP)
}



#' Calculate Livelihoods subgoal of Coastal Livelihoods and Economies.
#' 
#' @param jc total adjusted jobs per sector at current time
#' @param jr total adjusted jobs per sector at reference time
#' @param gc average PPP-adjusted per-capita annual wages per sector in current region
#' @param gr average PPP-adjusted per-capita annual wages per sector in reference region
#' @return 
#' @export
#' 
Halpern2012.LE.LIV = function (jc, jr, gc, gr, ...) {
    xLIV = ( (sum(jc) / sum(jr)) + (sum(gc) / sum(gr)) ) / 2.0
    
    return (xLIV)
}

#' Calculate Economies subgoal of Coastal Livelihoods and Economies.
#' 
#' @param ec total adjusted revenue generated per sector at current time
#' @param er total adjusted revenue generated per sector at reference time
#' @return 
#' @export
#' 
Halpern2012.LE.ECO = function (ec, er, ...) {
    xECO = sum( ec / er )
    
    return (xECO)
}

#' Calculate Coastal Livelihoods and Economies.
#' 
#' @param jc total adjusted jobs per sector at current time
#' @param jr total adjusted jobs per sector at reference time
#' @param gc average PPP-adjusted per-capita annual wages per sector in current region
#' @param gr average PPP-adjusted per-capita annual wages per sector in reference region
#' @param ec total adjusted revenue generated per sector at current time
#' @param er total adjusted revenue generated per sector at reference time
#' @return 
#' @export
#' 
Halpern2012.LE = function (jc, jr, gc, gr, ec, er, ...) {
    xLIV = Halpern2012.LE.LIV(jc, jr, gc, gr)
    xECO = Halpern2012.LE.LIV(ec, er)
    
    xLE = (xLIV + xECO) / 2.0
    
    return (xLE)
}



#' Calculate Tourism and Recreation.
#' 
#' @param D number of tourist-days
#' @param t most recent year
#' @param V total region population size
#' @param S sustainability factor
#' @return 
#' @export
#' 
Halpern2012.TR = function (D, t, V, S, ...) {
    xTR = log10( ( ( D / V ) * S ) + 1 )
    return (xTR)
}



#' Calculate Iconic Species subgoal of Sense of Place.
#' 
#' @param S number of assessed species in each category
#' @param w status weight assigned per threat category
#' @return 
#' @export
#' 
Halpern2012.ICO = function (S, w, ...) {
    return (stats::weighted.mean(S, w, na.rm=T))
}

#' Calculate Lasting Special Places subgoal of Sense of Place.
#' 
#' @param CMPA coastal marine protected area
#' @param tCMPA total coastal marine area
#' @param CP coastline protected
#' @param tCP total coastline
#' @return 
#' @export
#' 
Halpern2012.LSP = function (CMPA, tCMPA, CP, tCP, ...) {
    xLSP = ( (CMPA / (0.3*tCMPA) + (CP / (0.3*tCP)) ) / 2.0
    return (xLSP)
}

#' Calculate Sense of Place.
#' 
#' @param S number of assessed species in each category
#' @param w status weight assigned per threat category
#' @param CMPA coastal marine protected area
#' @param tCMPA total coastal marine area
#' @param CP coastline protected
#' @param tCP total coastline
#' @return 
#' @export
#' 
Halpern2012.SP = function (S, w, CMPA, tCMPA, CP, tCP, ...) {
    xICO = Halpern2012.ICO(S, w)
    xLSP = Halpern2012.LSP(CMPA, tCMPA, CP, tCP)
    
    xSP = (xICO + xLSP) / 2.0
    
    return (xSP)
}



#' Calculate Clean Waters.
#' 
#' @param a number of coastal people without access to sanitation rescaled to global maximum
#' @param u 1 - (nutrient input)
#' @param l 1 - (chemical input)
#' @param d 1 - (marine debris input)
#' @return 
#' @export
#' 
Halpern2012.CW = function (a, u, l, d, ...) {
    xCW = ( a * u * l * d )^(1/4)
    
    return (xCW)
}



#' Calculate Species subgoal of Biodiversity.
#' 
#' @param 
#' @return 
#' @export
#' 
Halpern2012.BD.SPP = function (A, G, w, ...) {
    sub.cells = cbind(G, A, w)
    
    super.cells = plyr::ddply(
        grid_cells,
        'G',
        function (x) { return ( c('A' = mean(x$A), 'cell' = (sum(x$w) / length(x$w)) ) ) }
    )
    
    xSPP = sum(super.cells$cell * super.cells$A) / sum(super.cells$A)
    
    return (xSPP)
}

#' Calculate Habitats subgoal of Biodiversity.
#' 
#' @param 
#' @return 
#' @export
#' 
Halpern2012.BD.HAB = function (Cc, Cr, ...) {
    xHAB = sum( Cc / Cr ) / length(Cc)
    
    return (xHAB)
}

#' Calculate Biodiversity.
#' 
#' @param 
#' @return 
#' @export
#' 
Halpern2012. = function (A, G, w, Cc, Cr, ...) {
    xSPP = Halpern2012.BD.SPP(A, G, w)
    xHAB = Halpern2012.BD.HAB(Cc, Cr)
    
    xBD = (xSPP + xHAB) / 2.0
    
    return (xBD)
}





























