##' Plot Aster
##' 
##' Plot flower plot. Created by Jim Regetz. Slight modifications by Darren and
##' Ben.
##' 
##' 
##' @param lengths length of petal outward to extent of circle
##' @param widths width of petal
##' @param labels petal label outside of circel
##' @param disk relative radius of a central donut hole
##' @param max.length ...
##' @param center center value
##' @param main
##' @param fill.col
##' @param plot.outline
##' @param label.offset
##' @param xlim
##' @param ylim
##' @param uin
##' @param tol
##' @param cex
##' @param bty
##' @param lty
##' @param label.col
##' @param label.font
##' @param label.cex
##' @return Generate something akin to a rose plot in which the width and
##' length of each petal are directly specified by the user. Or to put it
##' differently, this is somewhat like a pie chart in which the radius of each
##' wedge is allowed to vary (along with the angular width, as pie charts do).
##' As an additional enhancement, one can specify a central disk of arbitrary
##' radius (from 0 to 1, assuming that the plot itself is scaled to the unit
##' circle), in which case the petal heights are always measured from the edge
##' of the disk rather than the center of the circle; if desired, text can be
##' added in the center.
##' 
##' Although this kind of plot may already be well known in some circles (no
##' pun intended), I haven't seen it clearly defined or labeled anywhere, so
##' I'm anointing it an 'aster' plot because its component parts are
##' reminiscent of composite flower morphology.
##' 
##' The 'lengths' dictates how far out each petal extends, 'widths' dictates
##' the (angular) width of each petal, and 'disk' gives the relative radius of
##' a central donut hole. If no widths are provided, all petals will have equal
##' widths. Additional function arguments can also control whether petals are
##' labeled, whether the petal lengths are rescaled to the maximum score or to
##' a user-input score, whether spokes delineating each petal are extended to
##' an outer circle, and more. I also wrote a quick convenience wrapper for
##' creating a legend plot.
##' 
##' Note that the function here is a repurposed and very heavily modified
##' version of the windrose() function contained in the 'circular' package,
##' although sufficiently rewritten so as not to depend on any functionality in
##' that package.
##' @keywords layers_navigation
##' @examples
##' 
##' \dontrun{
##' # generate some fake data
##' set.seed(1)
##' scores <- sample(1:10)
##' weights <- sample(1:10)
##' labels <- paste(LETTERS[1:10], "X", sep="")
##' 
##' # do some plots
##' par(mfrow=c(2,2), xpd=NA)
##' aster(lengths=scores, widths=weights, disk=0, main="Example 1",
##'     plot.outline=FALSE)
##' aster(lengths=scores, widths=weights, labels=labels, main="Example 2",
##'     lty=2, fill.col="gray", plot.outline=FALSE)
##' aster.legend(labels=labels, widths=weights)
##' aster(lengths=scores, widths=weights, disk=0.5, main="Example 3",
##'     center="Hello world")
##' }
##' 
##' @export aster
aster = function (lengths, widths, labels, disk=0.5, max.length,
                  center=NULL, main=NULL, fill.col=NULL, plot.outline=TRUE,
                  label.offset=0.15, xlim=c(-1.2, 1.2), ylim=c(-1.2, 1.2), uin=NULL,
                  tol=0.04, cex=1, bty="n", lty=1, 
                  label.col='black', label.font=3, label.cex=NULL, ...) {
  
  # Custom R function to generate something akin to a rose plot in which
  # the width and length of each petal are directly specified by the user.
  # Or to put it differently, this is somewhat like a pie chart in which
  # the radius of each wedge is allowed to vary (along with the angular
  # width, as pie charts do). As an additional enhancement, one can
  # specify a central disk of arbitrary radius (from 0 to 1, assuming that
  # the plot itself is scaled to the unit circle), in which case the petal
  # heights are always measured from the edge of the disk rather than the
  # center of the circle; if desired, text can be added in the center.
  #
  # Although this kind of plot may already be well known in some circles
  # (no pun intended), I haven't seen it clearly defined or labeled
  # anywhere, so I'm anointing it an 'aster' plot because its component
  # parts are reminiscent of composite flower morphology.
  #
  # As coded below, 'lengths' dictates how far out each petal extends,
  # 'widths' dictates the (angular) width of each petal, and 'disk' gives
  # the relative radius of a central donut hole. If no widths are
  # provided, all petals will have equal widths. Additional function
  # arguments can also control whether petals are labeled, whether the
  # petal lengths are rescaled to the maximum score or to a user-input
  # score, whether spokes delineating each petal are extended to an outer
  # circle, and more. I also wrote a quick convenience wrapper for
  # creating a legend plot.
  #
  # Note that the function here is a repurposed and very heavily modified
  # version of the windrose() function contained in the 'circular'
  # package, although sufficiently rewritten so as not to depend on any
  # functionality in that package.
  #
  # Example invocations appear below.
  #
  # Jim Regetz
  # NCEAS
  # Created on 13-Sept-2011
  #
  # Mods by Ben Best and Darren Hardy
  # December 2011
  #  - fix blank hairlines between circles and polygons in pedals
  #  - accepts more labeling and title options
  #  - accepts data frames for lengths
  #
  # Example plots...
  #
  # # generate some fake data
  # set.seed(1)
  # scores <- sample(1:10)
  # weights <- sample(1:10)
  # labels <- paste(LETTERS[1:10], "X", sep="")
  # 
  # # do some plots
  # png(file="aster-plots.png", height=600, width=600)
  # par(mfrow=c(2,2), xpd=NA)
  # aster(lengths=scores, widths=weights, disk=0, main="Example 1",
  #     plot.outline=FALSE)
  # aster(lengths=scores, widths=weights, labels=labels, main="Example 2",
  #     lty=2, fill.col="gray", plot.outline=FALSE)
  # aster.legend(labels=labels, widths=weights)
  # aster(lengths=scores, widths=weights, disk=0.5, main="Example 3",
  #     center="Hello world")
  # dev.off()
  # main aster function definition
  
  if (is.data.frame(lengths)) {
    lengths <- as.numeric(lengths)
  }
  n.petals <- length(lengths)
  if (missing(widths)) {
    widths <- rep(1, n.petals)
  }
  if (missing(max.length)) {
    max.length <- max(lengths)
  }
  if (missing(labels)) {
    labels <- names(lengths)
  }
  if (missing(label.cex)) {
    label.cex <- 0.7 * cex
  }  
  
  # determine radius of each petal
  if (disk < 0 || 1 < disk) {
    stop("disk radius must be between 0 and 1")
  }
  radii <- disk + (1-disk) * lengths/max.length
  
  # define inner function for drawing circles
  # (from original windrose function)
  circles <- function(rad, sector=c(0, 2 * pi), lty=2,
                      col="white", border=NA, fill=FALSE) {
    values <- seq(sector[1], sector[2], by=(sector[2] - sector[1])/360)
    x <- rad * cos(values)
    y <- rad * sin(values)
    if (fill) {
      polygon(x, y, xpd=FALSE, lty=lty, col=col, border=border)
    }
    lines(x, y, col=1, lty=lty)
  }
  
  # lots of low-level positional details
  # (from original windrose function)
  op <- par(mar=c(1, 1, 2, 1))
  mai <- par("mai")
  on.exit(par(op))
  midx <- 0.5 * (xlim[2] + xlim[1])
  xlim <- midx + (1 + tol) * 0.5 * c(-1, 1) * (xlim[2] - xlim[1])
  midy <- 0.5 * (ylim[2] + ylim[1])
  ylim <- midy + (1 + tol) * 0.5 * c(-1, 1) * (ylim[2] - ylim[1])
  oldpin <- par("pin") - c(mai[2] + mai[4], mai[1] + mai[3])
  xuin <- oxuin <- oldpin[1]/diff(xlim)
  yuin <- oyuin <- oldpin[2]/diff(ylim)
  if (is.null(uin)) {
    if (yuin > xuin) {
      xuin <- yuin
    } else {
      yuin <- xuin
    }
  } else {
    if (length(uin) == 1)
      uin <- uin * c(1, 1)
    if (any(c(xuin, yuin) < uin))
      stop("uin is too large to fit plot in")
    xuin <- uin[1]
    yuin <- uin[2]
  }
  xlim <- midx + oxuin/xuin * c(-1, 1) * diff(xlim) * 0.5
  ylim <- midy + oyuin/yuin * c(-1, 1) * diff(ylim) * 0.5
  
  # generate breaks (petal boundaries) based on the widths
  breaks <- (2*pi*c(0, cumsum(widths))/sum(widths))[-(n.petals+1)]
  breaks <- c(breaks, 2 * pi)
  plot(c(-1.2, 1.2), c(-1.2, 1.2), xlab="", ylab="", main="",
       xaxt="n", yaxt="n", pch=" ", xlim=xlim, ylim=ylim,
       bty=bty, ...)
  title(main=main, ...)
  
  # plot full petal outlines
  if (plot.outline) {
    # note: go to n.petals not n.breaks because we the last break is
    # the same as the first
    for (i in 1:n.petals) {
      lines(c(0, cos(breaks[i])), c(0, sin(breaks[i])), lty=lty)
    }
    circles(1, lty=lty)
  }
  # plot the petals themselves
  if (is.null(fill.col)) {
    fill.col <- rainbow(n.petals)
  }
  fill.col <- rep(fill.col, length.out=n.petals)
  for (i in 1:n.petals) {
    w1 <- breaks[i]
    w2 <- breaks[i + 1]
    rad <- radii[i]
    xx <- rad * c(0, cos(w1), cos(w2), 0)
    yy <- rad * c(0, sin(w1), sin(w2), 0)
    polygon(xx, yy, xpd=FALSE, col=fill.col[i], border=fill.col[i])
    lines(xx[1:2], yy[1:2])
    lines(xx[3:4], yy[3:4])
    circles(rad=rad, sector=c(w1, w2), fill=TRUE,
            lty=1, col=fill.col[i], border=fill.col[i])
  }
  # plot petal labels, if given
  if (!is.null(labels)) {
    if (plot.outline) {
      height <- label.offset + rep(1, n.petals)
    } else {
      height <- label.offset + radii
    }
    mids <- breaks[1:n.petals] + diff(breaks)/2
    for (i in 1:n.petals) {
      text(height[i] * cos(mids[i]), height[i] * sin(mids[i]),
           labels=labels[i], cex=label.cex, 
           font=label.font, col=label.col)
    }
  }
  
  # add disk, if desired, with optional text in the middle
  if (0 < disk) {
    circles(disk, fill=TRUE, lty=1)
  }
  if (!is.null(center)) {
    text(0, 0, labels=center, font=2, cex=2.2*cex)
  }
  invisible(NULL)
}