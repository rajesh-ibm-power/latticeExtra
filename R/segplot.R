



## Want to plot intervals from start to end, color coded by decoded, 
## and several in a panel  according to source

## since there are colors involved and levelplot already 
## has a colorkey, we'll use that





prepanel.segplot <- 
    function(x, y, z, subscripts, horizontal = TRUE, ...)
{
    ans <- 
        list(xlim = range(x[subscripts], y[subscripts], finite = TRUE), 
             ylim =
             if (is.numeric(z)) range(z[subscripts], finite = TRUE)
             else levels(z))
    if (horizontal) ans
    else with(ans, list(xlim = ylim, ylim = xlim))
}


panel.segplot <- 
    function(x, y, z, level = NULL, subscripts,
             at,
             draw.bands = is.factor(z),
             col = if (draw.bands) plot.polygon$col else plot.line$col,
             alpha = if (draw.bands) plot.polygon$alpha else plot.line$alpha,
             lty = if (draw.bands) plot.polygon$lty else plot.line$lty,
             lwd = if (draw.bands) plot.polygon$lwd else plot.line$lwd,
             border = if (draw.bands) plot.polygon$border else "transparent",
             col.regions = regions$col,
             band.height = 0.6,
             horizontal = TRUE,
             ...,
             centers = NULL,
             pch = 16)
{
    plot.line <- trellis.par.get("plot.line")
    plot.polygon <- trellis.par.get("plot.polygon") 
    regions <- trellis.par.get("regions")
    x1 <- as.numeric(x[subscripts])
    x2 <- as.numeric(y[subscripts])
    z <- z[subscripts]
    if (!is.null(level))
    {
        ## col is overridden
        level <- as.numeric(level[subscripts])
        col <- level.colors(level, at, col.regions, colors = TRUE)
    }
    if (draw.bands)
    {
        if (horizontal)
            panel.rect(x = 0.5 * (x1 + x2),
                       width = x2 - x1,
                       y = as.numeric(z), height = band.height,
                       border = border, col = col, alpha = alpha,
                       lty = lty, lwd = lwd,
                       ...)
        else 
            panel.rect(y = 0.5 * (x1 + x2),
                       height = x2 - x1,
                       x = as.numeric(z), width = band.height,
                       border = border, col = col, alpha = alpha,
                       lty = lty, lwd = lwd,
                       ...)
    }
    else
    {
        if (horizontal)
            panel.segments(x1, as.numeric(z), x2, as.numeric(z), 
                           col = col, alpha = alpha, lty = lty, lwd = lwd,
                           ...)
        else
            panel.segments(as.numeric(z), x1, as.numeric(z), x2,
                           col = col, alpha = alpha, lty = lty, lwd = lwd,
                           ...)

    }
    if (!is.null(centers))
    {
        if (horizontal)
            panel.points(x = as.numeric(centers[subscripts]),
                         y = as.numeric(z),
                         pch = pch, ...)
        else
            panel.points(y = as.numeric(centers[subscripts]),
                         x = as.numeric(z),
                         pch = pch, ...)

    }
}


segplot <- function(x, data, ...) UseMethod("segplot")


segplot.formula <- 
    function(x, data = parent.frame(),
             level = NULL, centers = NULL,
             prepanel = prepanel.segplot,
             panel = panel.segplot,
             xlab = NULL, ylab = NULL,
             horizontal = TRUE,
             ...,
             at, cuts = 30, colorkey = !is.null(level))
{
    level <- eval(substitute(level), data, parent.frame())
    centers <- eval(substitute(centers), data, parent.frame())
    if (!is.null(level))
    {
        rng <- lattice:::extend.limits(range(as.numeric(level), finite = TRUE))
        if (missing(at)) at <- do.breaks(rng, cuts + 1)
    }
    levelplot(x, data, level = level, centers = centers,
              ...,
              default.scales = 
              if (horizontal) list(y = list(alternating = FALSE, tck = 0))
              else list(x = list(alternating = FALSE, tck = 0)),
              xlab = xlab,
              ylab = ylab,
              at = at,
              colorkey = colorkey,
              horizontal = horizontal,
              prepanel = prepanel, 
              panel = panel)
}


## green.red <- function(n, gamma = 1, power = 1)
## {
##     m <- ceiling(n/2)
##     c(hsv(h = 0.33, s = seq(1, 0, length = m)^power, gamma = gamma),
##       hsv(h = 1, s = seq(0, 1, length = m)^power, gamma = gamma))
## }



## segplot(1:100 ~ rnorm(100) + runif(100), data = parent.frame())


