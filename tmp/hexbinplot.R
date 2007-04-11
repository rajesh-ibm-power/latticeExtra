

## lattice version of gplot.hexbin

## There are two major problems.  (1) For comparability across panels,
## we want the same mincnt and maxcnt in all panels.  However, a
## suitable default can really only be determined at printing time,
## since it would depend on the physical dimensions of the panel.  (2)
## there is no proper way to communicate the mincnt and maxcnt to the
## legend.

## Tentative solution: the counts can be calculated once enough things
## are known, namely the aspect ratio, xbins and [xy]bnds.  An
## important question then is whether [xy]bnds should be [xy]lim or
## range([xy]).  Both should be allowed, since [xy]lim makes them
## comparable, range([xy]) potentially shows more detail.  For
## relation != "same", both are more or less similar.  An important
## observation is that with range([xy]), 'shape = aspect ratio of
## panel' does not guarantee symmetric hexagons, so shape has to be
## different for each panel.

## Only feasible approach I can think of is to produce the trellis
## object first (with known aspect, so aspect="fill" is absolutely
## no-no), then analyze the limits and relevant panel arguments to get
## 'maxcnt' (essentially doing a dry run of the panel calculations).
## This needs undocumented knowledge of the trellis object, which is
## kinda not good, but at least it gets the job done.  Once we know
## maxcnt, we can also set up a suitable legend function.

## Unfortunately, this has the potential to screw up update calls that
## modify certain things.  Is there any way to capture those?  Maybe
## make a new class that inherits from "trellis".  For now, we'll
## pretend that the problem doesn't exist.



prepanel.hexbinplot <-
    function(x, y, ...)
{
    ans <-
        list(xlim = range(x, finite = TRUE),
             ylim = range(y, finite = TRUE),
             dx = IQR(x),
             dy = IQR(y))
}




panel.hexbinplot <-
    function(x, y,
             xbins = 30,
             xbnds = c("panel", "data"),
             ybnds = c("panel", "data"),

             ## special args
             .prelim = FALSE,
             .cpl = current.panel.limits(),
             .xlim = .cpl$xlim,
             .ylim = .cpl$ylim,
             .aspect.ratio,

             type = character(0),
             ...,
             check.erosion = FALSE)
{
    if (is.character(xbnds))
        xbnds <-
            switch(match.arg(xbnds),
                   panel = .xlim,
                   data = range(x, finite = TRUE))
    if (is.character(ybnds))
        ybnds <-
            switch(match.arg(ybnds),
                   panel = .ylim,
                   data = range(y, finite = TRUE))
    shape <-
        .aspect.ratio * (diff(ybnds) / diff(.ylim)) /
            (diff(xbnds) / diff(.xlim))
    if (!missing(check.erosion))
        warning("explicit 'check.erosion' specification ignored")
    h <- hexbin(x = x, y = y,
                xbins = xbins, shape = shape,
                xbnds = xbnds, ybnds = ybnds)
    if (.prelim)
        return(max(h@count))
    
    ## have to do this because grid.hexagons croaks with unrecognized
    ## arguments:
    args <- list(dat = h, check.erosion = FALSE, ...)
    keep <- names(args) %in% names(formals(grid.hexagons))

    if ('g' %in% type) panel.grid(h = -1, v = -1)

    do.call("grid.hexagons", args[keep])

    if ("r" %in% type) panel.lmline(x, y, ...)
    if ("smooth" %in% type) panel.loess(x, y, ...)
    invisible()
}




hexbinplot <-
    function(x, ...)
    UseMethod("hexbinplot")




hexbinplot.formula <-
    function(x, data = parent.frame(),
             prepanel = prepanel.hexbinplot,
             panel = panel.hexbinplot,
             aspect = "xy",
             trans = NULL,
             inv = NULL,
             colorkey = TRUE,
             ...,
             maxcnt,
             legend = NULL,
             legend.width = TRUE)
{
    if (is.logical(legend.width)) legend.width <- 1.2 * as.numeric(legend.width)
    if (is.character(aspect) && aspect == "fill")
        stop("aspect = 'fill' not permitted")
    if (!is.null(trans) && is.null(inv))
        stop("Must supply the inverse transformation 'inv'")
    ans <-
        xyplot(x, data = data,
               prepanel = prepanel,
               panel = panel,
               aspect = aspect,
               trans = trans,
               inv = inv,
               legend = legend,
               ...)
    ## panel needs to know aspect ratio to calculate shape
    ans <- update(ans, .aspect.ratio = ans$aspect.ratio)

    ## also need maxcnt, o.w. can't draw legend, panels not comparable
    ## either
    if (missing(maxcnt))
        maxcnt <- 
            max(mapply(panel.hexbinplot, ## note: not 'panel'
                       x = lapply(ans$panel.args, "[[", "x"),
                       y = lapply(ans$panel.args, "[[", "y"),
                       .xlim =
                       if (is.list(ans$x.limits)) ans$x.limits
                       else rep(list(ans$x.limits), length(ans$panel.args)),
                       .ylim =
                       if (is.list(ans$y.limits)) ans$y.limits
                       else rep(list(ans$y.limits), length(ans$panel.args)),
                       MoreArgs =
                       c(ans$panel.args.common,
                         list(.prelim = TRUE, .cpl = NA))))
    ans <- update(ans, maxcnt = maxcnt)
    if (colorkey)
        ans <-
            update(ans,
                   legend = lattice:::updateList(ans$legend,
                   list(right =
                        list(fun = hexlegendGrob,
                             args =
                             list(maxcnt = maxcnt,
                                  trans = trans,
                                  inv = inv,
                                  legend = legend.width,
                                  ...)))))
    ans
}


## want a grob instead of actual plotting

hexlegendGrob <-
    function(legend = 1.2,
             inner = legend / 5,
             cex.labels = 1,
             cex.title = 1.2,
             style = "colorscale",
             minarea = 0.05, maxarea = 0.8,
             mincnt = 1, maxcnt,
             trans = NULL, inv = NULL,
             colorcut = seq(0, 1, length = 17),
             density = NULL, border = NULL, pen = NULL,
             colramp = function(n) { LinGray(n,beg = 90,end = 15) },
             ...,
             vp = NULL,
             draw = FALSE)
{
    ## the formal arg matching should happen
    style <- match.arg(style, eval(formals(grid.hexagons)[["style"]]))
    if (style %in% c("centroids", "lattice", "colorscale")) {
	## _______________tranformations_______________________
	if(is.null(trans)) {
	    sc <- maxcnt - mincnt
	    bnds <- round(mincnt + sc * colorcut)
	}

	else {
	    if(!is.function(trans) && !is.function(inv))
		stop("'trans' and 'inv' must both be functions if 'trans' is not NULL")
	    con <- trans(mincnt)
	    sc <- trans(maxcnt) - con
	    bnds <- round(inv(con + sc * colorcut))
	}
    }

    ## grob
    ans <- 
        switch(style,
               "colorscale" = {

                   n <- length(bnds)
                   pen <- colramp(n-1)

                   ## rectangles instead of polygons
                   ## pol <-
                   ##     rectGrob(x = 0.5, y = 1:(n-1)/n,
                   ##              height = 1/n,
                   ##              default.units = "npc",
                   ##              gp = gpar(fill = pen, col = border))

                   hexxy <- hexcoords(dx = 1, n = 1)[c("x", "y")]
                   maxxy <- max(abs(unlist(hexxy)))
                   hexxy <- lapply(hexxy, function(x) 0.5 * x/ maxxy)

                   pol <- 
                       polygonGrob(x = 0.5 + rep(hexxy$x, n-1),
                                   y = (rep(1:(n-1), each = 6) + hexxy$y) / n,
                                   id.lengths = rep(6, n-1),
                                   gp = gpar(fill = pen, col = border),
                                   default.units = "npc")
                   txt <- 
                       textGrob(as.character(bnds),
                                x = 0.5,
                                y = (0:(n-1) + 0.5) / n,
                                gp = gpar(cex = cex.labels),
                                default.units = "npc")
                   ttl <- textGrob("Counts", gp = gpar(cex = cex.title))

                   key.layout <-
                       grid.layout(nrow = 2, ncol = 2,
                                   heights =
                                   unit(c(1.5, 1),
                                        c("grobheight", "grobheight"),
                                        data = list(ttl, txt)),
                                   widths =
                                   unit(c(1/n, 1),
                                        c("grobheight", "grobwidth"),
### FIXME: should be: data = list(pol, txt)), when grid bug is fixed
                                        data = list(txt, txt)),
                                   respect = TRUE)
                   key.gf <- frameGrob(layout = key.layout, vp = vp)
                   key.gf <- placeGrob(key.gf, ttl, row = 1, col = 1:2)
                   key.gf <- placeGrob(key.gf, pol, row = 2, col = 1)
                   key.gf <- placeGrob(key.gf, txt, row = 2, col = 2)
                   key.gf
               },

               "centroids" = ,
               "lattice" = {

                   warning("legend shows relative sizes")

                   ## Note: it may not be impossible to get absolute
                   ## sizes.  The bigger problem is that when
                   ## [xy]bnds="data", the sizes (for the same count) may
                   ## not be the same across panels.  IMO, that's a more
                   ## useful feature than getting the absolute sizes
                   ## right.

                   radius <- sqrt(minarea + (maxarea - minarea) * colorcut)
                   n <- length(radius)
                   if(is.null(pen)) pen <- 1
                   if(is.null(border)) border <- pen

                   hexxy <- hexcoords(dx = 1, n = 1)[c("x", "y")]
                   maxxy <- max(abs(unlist(hexxy)))
                   hexxy <- lapply(hexxy, function(x) 0.5 * x/ maxxy)

                   pol <- 
                       polygonGrob(x = 0.5 + rep(radius, each = 6) * rep(hexxy$x, n),
                                   y = (rep(0.5 + 1:n, each = 6) +
                                        rep(radius, each = 6) * hexxy$y - 1) / n,
                                   id.lengths = rep(6, n),
                                   gp = gpar(fill = pen, col = border),
                                   default.units = "npc")
                   txt <- 
                       textGrob(as.character(bnds),
                                x = 0.5,
                                y = (1:n - 0.5) / n,
                                gp = gpar(cex = cex.labels),
                                default.units = "npc")
                   ttl <- textGrob("Counts", gp = gpar(cex = cex.title))

                   key.layout <-
                       grid.layout(nrow = 2, ncol = 2,
                                   heights =
                                   unit(c(1.5, 1),
                                        c("grobheight", "grobheight"),
                                        data = list(ttl, txt)),
                                   widths =
                                   unit(c(1/n, 1),
                                        c("grobheight", "grobwidth"),
### FIXME: should be: data = list(pol, txt)), when grid bug is fixed
                                        data = list(txt, txt)),
                                   respect = TRUE)
                   key.gf <- frameGrob(layout = key.layout, vp = vp)

                   key.gf <- placeGrob(key.gf, ttl, row = 1, col = 1:2)
                   key.gf <- placeGrob(key.gf, pol, row = 2, col = 1)
                   key.gf <- placeGrob(key.gf, txt, row = 2, col = 2)
                   key.gf
               },

               "nested.lattice" = ,
               "nested.centroids" = {

                   dx <- inner/2
                   dy <- dx/sqrt(3)
                   hexC <- hexcoords(dx, dy, n = 1, sep = NULL)


                   ## _____________x scaling_____________________________
                   numb <- cut(floor(legend/inner), breaks = c(-1, 0, 2,4))
                   ## Note: In old code
                   ##	top breaks=c(-1,0,2,4,8), numb<- 5 and size=1:9
                   if(is.na(numb))
                       numb <- 4
                   switch(numb,
		      {
                          warning("not enough space for legend")
                          return(textGrob(""))
                      },
                          size <- 5,
                          size <- c(1, 5, 9),
                          size <- c(1, 3, 5, 7, 9))
                   xmax <- length(size)
                   radius <- sqrt(minarea + (maxarea - minarea) * (size - 1)/9)
                   txt <- as.character(size)
                   ##___________________y scaling_____________________
                   lab <- c("Ones", "Tens", "Hundreds",
                            "Thousands", "10 Thousands", "100 Thousands",
                            "Millions", "10 Millions",
                            "100 Millions", "Billions")
                   power <- floor(log10(maxcnt)) + 1
                   yinc <- 16 * dy
                   ysize <- yinc * power
                   xmid <- 0
                   x <- inner * (1:xmax - (1 + xmax)/2) + xmid
                   n <- length(x)
                   tx <- rep.int(hexC$x, n)
                   ty <- rep.int(hexC$y, n)
                   six <- rep.int(6:6, n)
                   ## y <- rep.int(3 * dy - yinc, xmax)
                   y <- rep.int(3 * dy - 0.75 * yinc, xmax)


                   if (is.null(pen)) {
                       pen <- 1:power +1
                       pen <- cbind(pen, pen +10)
                   }
                   if (is.null(border)) border <- TRUE

                   key.layout <-
                       grid.layout(nrow = 1, ncol = 1,
                                   heights = unit(ysize, "inches"),
                                   widths = unit(legend, "inches"),
                                   respect = TRUE)
                   key.gf <- frameGrob(layout = key.layout, vp = vp)

                   ## for debugging
                   ## key.gf <-
                   ##     placeGrob(key.gf, rectGrob(gp = gpar(fill = "transparent")))

                   n6 <- rep.int(6, n)
                   for(i in 1:power) {

                       y <- y + yinc

                       key.gf <-
                           placeGrob(key.gf, 

                                     polygonGrob(x = unit(legend / 2 + rep.int(hexC$x, n) + rep.int(x, n6), "inches"),
                                                 y = unit(rep.int(hexC$y, n) + rep.int(y, n6), "inches"),
                                                 id.lengths = n6,
                                                 gp =
                                                 gpar(col = pen[i, 1], 
                                                      fill = if (border) 1 else pen[i, 1])),

                                     row = 1, col = 1)

                       key.gf <-
                           placeGrob(key.gf, 
                                     
                                     polygonGrob(x = legend / 2 + tx * rep.int(radius, six) + rep.int(x, six),
                                                 y = ty * rep.int(radius, six) + rep.int(y, six),
                                                 default.units = "inches", id=NULL,
                                                 id.lengths=rep(6,n),
                                                 gp = gpar(fill = pen[i,2], col = border)),
                                     
                                     row = 1, col = 1)

                       key.gf <-
                           placeGrob(key.gf, 
                                     
                                     textGrob(txt,
                                              x = legend / 2 + x,
                                              y = y - 4.5 * dy,
                                              default.units = "inches",
                                              gp = gpar(cex = cex.labels)),
                                     
                                     row = 1, col = 1)
                       key.gf <-
                           placeGrob(key.gf, 
                                     
                                     textGrob(lab[i],
                                              x = legend / 2 + xmid,
                                              y = y[1] + 4.5 * dy,
                                              default.units = "inches",
                                              gp = gpar(cex = 1.3 * cex.title)),

                                     row = 1, col = 1)
                   }
                   key.gf
               })
    if (draw)
    {
        grid.draw(ans)
        invisible(ans)
    }
    else ans
}




