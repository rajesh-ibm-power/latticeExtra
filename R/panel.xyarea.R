##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
##

panel.xyarea <- function(x, ...)
    UseMethod("panel.xyarea")

## Plot a series as a filled polygon connected at given origin (on y axis).
## With groups, acts like panel.superpose, but with polygon style settings.
panel.xyarea.default <-
    function(x, y, groups = NULL, origin = NULL,
             col = if (is.null(groups)) plot.polygon$col else superpose.polygon$col,
             border = if (is.null(groups)) plot.polygon$border else superpose.polygon$border,
             lty = if (is.null(groups)) plot.polygon$lty else superpose.polygon$lty,
             lwd = if (is.null(groups)) plot.polygon$lwd else superpose.polygon$lwd,
             alpha = if (is.null(groups)) plot.polygon$alpha else superpose.polygon$alpha,
             ..., col.line = border, fill, panel.groups = panel.xyarea)
{
    plot.polygon <- trellis.par.get("plot.polygon")
    superpose.polygon <- trellis.par.get("superpose.polygon")
    x <- as.numeric(x)
    y <- as.numeric(y)
    if (length(x) == 0) return()
    if (!is.null(groups)) {
        ## NOTE superpose does not handle 'border' argument, so pass it as col.line
        panel.superpose(x, y, ..., groups = groups, panel.groups = panel.groups,
                        col = col, col.line = border, lty = lty, lwd = lwd,
                        alpha = alpha, origin = origin)
    } else {
        if (all(is.na(col)) && !missing(col.line))
            col <- col.line
        xx <- c(head(x,1), x, tail(x,1))
        if (is.null(origin))
            origin <- current.panel.limits()$ylim[1]
        yy <- c(origin, y, origin)
        ## we need to catch the 'fill' argument from panel.superpose, otherwise over-rides 'col'
        panel.polygon(xx, yy, alpha = alpha, col = col, border = col.line, lty = lty, lwd = lwd, ...)
    }
}

panel.xyarea.zoo <-
panel.xyarea.ts <- function(x, y = NULL, ...)
{
    if (!is.null(y)) {
        panel.xyarea.default(x, y, ...)
    } else {
        panel.xyarea.default(as.vector(time(x)), as.vector(x), ...)
    }
}

## A slightly modified copy of panel.qqmath
panel.qqmath.xyarea <-
    function(x, y = NULL, f.value = NULL, distribution = qnorm, qtype = 7,
             groups = NULL, ...)
{
    x <- as.numeric(x)
    distribution <- if (is.function(distribution))
        distribution
    else if (is.character(distribution))
        get(distribution)
    else eval(distribution)
    nobs <- sum(!is.na(x))
    if (!is.null(groups))
        panel.xyarea(x, y = NULL, f.value = f.value, distribution = distribution,
                     qtype = qtype, groups = groups, ...,
                     panel.groups = panel.qqmath.xyarea)
    else if (nobs) {
        if (is.null(f.value))
            panel.xyarea(x = distribution(ppoints(nobs)), y = sort(x),
                ...)
        else panel.xyarea(x = distribution(if (is.numeric(f.value))
            f.value
        else f.value(nobs)), y = quantile(x, if (is.numeric(f.value))
            f.value
        else f.value(nobs), names = FALSE, type = qtype, na.rm = TRUE),
            ...)
    }
}
