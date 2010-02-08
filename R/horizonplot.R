##
## Copyright (c) 2010 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer


horizonplot <- function(x, data, ...)
    UseMethod("horizonplot")

horizonplot.default <-
    function(x, data = NULL, ...,
             panel = panel.horizonplot,
             prepanel = prepanel.horizonplot,
             strip = FALSE, strip.left = TRUE,
             par.strip.text = list(cex = 0.6),
             #layout = c(1, NA), ## TODO pending new lattice release
             groups = NULL,
             default.scales =
               list(y = list(relation = "sliced", axs = "i",
                             draw = FALSE, tick.number = 2)))
{
    if (!is.null(groups))
        stop("'groups' does not work in this plot")
    ans <- xyplot(x, data = data, ...,
                  panel = panel, prepanel = prepanel,
                  strip = strip, strip.left = strip.left,
                  par.strip.text = par.strip.text,
                  #layout = layout,
                  default.scales = default.scales)
    ans$call <- match.call()
    ans
}


panel.horizonplot <-
    function(x, y, ..., origin,
             border = NA, col.regions = regions$col)
{
    regions <- trellis.par.get("regions")
    origin <- current.panel.limits()$y[1]
    scale <- diff(current.panel.limits()$y)
    ## ordered for drawing, from least extreme to most extreme
    sections <- c(0, -1, 1, -2, 2, -3, 3, -4) ## these are the lower bounds
    ii <- quantile(seq_along(col.regions),
                   (sections - min(sections)) / (length(sections)-1),
                   type = 1)
    col <- col.regions[ii]
    for (i in seq_along(sections)) {
        section <- sections[i]
        yi <- y
        if (section < 0) {
            yi <- origin + origin - y
            section <- abs(section) - 1
        }
        baseline <- origin + section * scale
        if (all(yi <= baseline, na.rm = TRUE))
            next
        yi <- yi - baseline
        yi <- origin + pmax(pmin(yi, scale), 0)
        panel.xyarea(x, yi, border = border, col = col[i], ...)
    }
}

prepanel.horizonplot <-
    function(x, y, ..., origin = function(y) na.omit(y)[1])
{
    if (is.function(origin))
        origin <- origin(y)
    ans <- prepanel.default.xyplot(x, y, ...)
    scale <- max(abs(ans$ylim - origin)) / 3
    ans$ylim <- origin + c(0, scale)
    ans
}
