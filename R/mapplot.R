
prepanel.mapplot <- function(x, y, map, ...) {
    list(xlim = range(map$x, finite = TRUE),
         ylim = range(map$y, finite = TRUE))
}

panel.mapplot <-
    function(x, y, map, breaks, colramp,
             lwd = 0.5, ...)
{
    names(x) <- as.character(y)
    interval <-
        cut(x[map$names], breaks = breaks,
            labels = FALSE, include.lowest = TRUE)
    col.regions <- colramp(length(breaks) - 1)
    col <- col.regions[interval]
    panel.polygon(map, col = col, lwd = lwd, ...)
}


mapplot <- function(x, data, ...) UseMethod("mapplot")


mapplot.formula <-
    function(x, data, map, outer = TRUE,
             prepanel = prepanel.mapplot,
             panel = panel.mapplot,
             aspect = "iso",
             legend = NULL,
             breaks, cuts = 30,
             colramp = colorRampPalette(brewer.pal(n = 11, name = "Spectral")),
             colorkey = TRUE,
             ## col.regions,
             ## alpha.regions,
             ...)
{
    ccall <- match.call()
    ccall$data <- data
    ccall$map <- map
    ccall$outer <- outer
    ccall$prepanel <- prepanel
    ccall$panel <- panel
    ccall$aspect <- aspect
    ccall$legend <- legend
    ccall$colramp <- colramp
    ccall$default.scales <- list(x = list(tck = 1), y = list(tck = 1))
    ccall[[1]] <- quote(lattice::dotplot)
    ans <- eval(ccall, parent.frame())
    ans$call <- sys.call(sys.parent())
    ans$call[[1]] <- quote(mapplot)
    if (missing(breaks))
    {
        x <- unlist(lapply(ans$panel.args, "[[", "x"))
        breaks <-
            if (is.factor(x)) seq_len(1 + nlevels(x)) - 0.5
            else do.breaks(range(x, finite = TRUE), cuts)
    }
##     regions <- trellis.par.get("col.regions")
##     if (missing(col.regions)) col.regions <- regions$col
##     if (missing(alpha.regions)) alpha.regions <- regions$alpha
    if (colorkey)
        ans <-
            update(ans,
                   breaks = breaks,
                   legend = lattice:::updateList(ans$legend,
                   list(right = 
                        list(fun = draw.colorkey,
                             args = list(key = list(col = colramp(length(breaks)),
                                         at = breaks), 
                             draw = FALSE)))))
    ans
}



## mapplot(rownames(USCancerRates) ~ log(rate.male) + log(rate.female),
##         USCancerRates, outer = TRUE,
##         ## colramp = cm.colors,
##         map = county.map)

##         ## scales = list(draw = FALSE), xlab = "", ylab = "",
