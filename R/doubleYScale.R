##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

doubleYScale <-
    function(obj1, obj2, use.style = TRUE, add.axis = TRUE,
             add.ylab2 = FALSE, style1 = 1, style2 = 2,
             text = NULL, auto.key = if (!is.null(text))
               list(text, points = points, lines = lines, ...),
             points = FALSE, lines = TRUE, ...)
{
    stopifnot(inherits(obj1, "trellis"))
    stopifnot(inherits(obj2, "trellis"))

    ## force same x scales
    #xlim1 <- obj1$x.limits
    #if (is.list(xlim1))
    #    xlim1 <- rep(xlim1, length = prod(dim(obj2)))
    #obj2 <- update(obj2, xlim = xlim1, ylim = obj2$y.limits)
    ## TODO - ylim only here to workaround bug in lattice 0.17-15

    if (!is.null(auto.key)) {
        space <- "top"
        if (!is.null(auto.key$space))
            space <- auto.key$space
        auto.key$space <- NULL
        keyLeg <- list(space = list(fun = "drawSimpleKey",
                       args = auto.key))
        names(keyLeg) <- space
        obj1 <- update(obj1, legend = keyLeg)
    }
    if (add.ylab2) {
        ## add ylab2 as a 'legend' (idea from John Maindonald)
        ## draw both ylabs in their style, if specified
        ylabStyledGrob <- function(label, style) {
            textGrob(label, y = 0.5, rot = 90,
                     gp = if (isTRUE(style > 0))
                     gpar(col = trellis.par.get("superpose.line")$col[style]))
        }
        is.characterOrExpression <- function(x)
            is.character(x) || is.expression(x)

        if (use.style && isTRUE(style1 > 0)) {
            ylab1 <- obj1$ylab
            if (is.list(ylab1))
                ylab1 <- obj1$ylab.default
            if (is.characterOrExpression(ylab1)) {
                obj1 <-
                    update(obj1, legend = list(left =
                                 list(fun = ylabStyledGrob,
                                      args = list(label = ylab1,
                                      style = style1))))
                obj1$ylab <- expression(NULL)
            }
        }
        if (use.style == FALSE)
            style2 <- 0
        ylab2 <- obj2$ylab
        if (is.list(ylab2))
            ylab2 <- obj2$ylab.default
        if (is.characterOrExpression(ylab2)) {
            obj1 <-
                update(obj1, legend = list(right =
                             list(fun = ylabStyledGrob,
                                  args = list(label = ylab2,
                                  style = style2))))
        }
    }

    if (add.axis == FALSE) {
        ## if not drawing a second axis, nothing to do but...
        return(obj1 + as.layer(obj2, x.same = TRUE, y.same = FALSE,
                               axes = NULL,
                               style = if (use.style) style2))
    }

    ## need to specify padding to draw second y axis
    yAxPad <- list(layout.widths = list(
                   axis.left = list(x = 2.5, units = "char"),
                   axis.right = list(x = 3, units = "char")))

    dummy <- update(obj1, panel = function(...) NULL,
                    scales = list(y = list(draw = FALSE)),
                    lattice.options = yAxPad)
    dummy +
        as.layer(obj1, style = if (use.style) style1,
                 x.same = TRUE, y.same = FALSE,
                 axes = "y", out = TRUE, opp = FALSE) +
            as.layer(obj2, style = if (use.style) style2,
                     x.same = TRUE, y.same = FALSE,
                     axes = "y", out = TRUE, opp = TRUE)
}


as.layer.trellis <-
    function(x,
             x.same = TRUE,
             y.same = TRUE,
             axes = c(if (!x.same) "x", if (!y.same) "y"),
             opposite = TRUE,
             outside = FALSE,
             ...)
{
    if (identical(axes, TRUE)) axes <- c("x", "y")
    if (identical(axes, FALSE)) axes <- NULL
    opposite <- rep(opposite, length = 2)
    outside <- rep(outside, length = 2)
    if (x.same && y.same) {
        ## simply run the panel function in existing panel viewport
        return(
               layer({
                   packet.number <- min(packet.number(), prod(dim(x)))
                   do.call(x$panel, trellis.panelArgs(x, packet.number))
               }, data = list(x = x), ...)
               )
    }
    ## else
    ## take one or more scales from layered object (so new viewport)
    ## draw panels and axes from this trellis object
    layer({
        packet.number <- min(packet.number(), prod(dim(x)))
        ## axis details...
        ## this all copied from lattice:::plot.trellis
        x.relation.same <- x$x.scales$relation == "same"
        y.relation.same <- x$y.scales$relation == "same"
        xscale.comps <-
            if (x.relation.same)
                x$xscale.components(lim = x$x.limits,
                                    top = TRUE,
                                    ## rest passed on to
                                    ## calculateAxisComponents
                                    ## in the default
                                    ## case:
                                    at = x$x.scales$at,
                                    used.at = x$x.used.at,
                                    num.limit = x$x.num.limit,
                                    labels = x$x.scales$lab,
                                    logsc = x$x.scales$log,
                                    abbreviate = x$x.scales$abbr,
                                    minlength = x$x.scales$minl,
                                    n = x$x.scales$tick.number,
                                    format.posixt = x$x.scales$format)
            else
                x$xscale.components(lim = x$x.limits[[packet.number]],
                                    top = FALSE,
                                    ## rest passed on to
                                    ## calculateAxisComponents
                                    ## in the default
                                    ## case:
                                    at = if (is.list(x$x.scales$at))
                                    x$x.scales$at[[packet.number]]
                                    else x$x.scales$at,
                                    used.at = x$x.used.at[[packet.number]],
                                    num.limit = x$x.num.limit[[packet.number]],
                                    labels =
                                    if (is.list(x$x.scales$lab))
                                    x$x.scales$lab[[packet.number]]
                                    else x$x.scales$lab,
                                    logsc = x$x.scales$log,
                                    abbreviate = x$x.scales$abbr,
                                    minlength = x$x.scales$minl,
                                    n = x$x.scales$tick.number,
                                    format.posixt = x$x.scales$format)

        yscale.comps <-
            if (y.relation.same)
                x$yscale.components(lim = x$y.limits,
                                    right = TRUE,
                                    ## rest passed on to
                                    ## calculateAxisComponents
                                    ## in the default
                                    ## case:
                                    at = x$y.scales$at,
                                    used.at = x$y.used.at,
                                    num.limit = x$y.num.limit,
                                    labels = x$y.scales$lab,
                                    logsc = x$y.scales$log,
                                    abbreviate = x$y.scales$abbr,
                                    minlength = x$y.scales$minl,
                                    n = x$y.scales$tick.number,
                                    format.posixt = x$y.scales$format)
            else
                x$yscale.components(lim = x$y.limits[[packet.number]],
                                    right = FALSE,
                                    ## rest passed on to
                                    ## calculateAxisComponents
                                    ## in the default
                                    ## case:
                                    at = if (is.list(x$y.scales$at))
                                    x$y.scales$at[[packet.number]]
                                    else x$y.scales$at,
                                    used.at = x$y.used.at[[packet.number]],
                                    num.limit = x$y.num.limit[[packet.number]],
                                    labels =
                                    if (is.list(x$y.scales$lab))
                                    x$y.scales$lab[[packet.number]]
                                    else x$y.scales$lab,
                                    logsc = x$y.scales$log,
                                    abbreviate = x$y.scales$abbr,
                                    minlength = x$y.scales$minl,
                                    n = x$y.scales$tick.number,
                                    format.posixt = x$y.scales$format)
        xscale <- xscale.comps$num.limit
        yscale <- yscale.comps$num.limit
        ## maybe over-ride with original limits
        if (x.same)
            x.scale <- current.panel.limits()$xlim
        if (y.same)
            y.scale <- current.panel.limits()$ylim
        ## do panel(); need a new viewport with scales from 'x'
        pushViewport(viewport(xscale = xscale, yscale = yscale))
        do.call(x$panel, trellis.panelArgs(x, packet.number))
        ## use axis components from the standard side only
        xscale.comps$top <- TRUE
        yscale.comps$right <- TRUE
        x.comp.list <- xscale.comps$bottom
        y.comp.list <- yscale.comps$left
        ## axes viewport (clip = "off" for outside axes)
        ## note: to draw an outside axis at top when there are stips:
        ## should really do it in strip.column.row.off
        pushViewport(viewport(xscale = xscale, yscale = yscale,
                              clip = "off"))
        if (("x" %in% axes) && is.list(x.comp.list) && x$x.scales$draw) {
            comp.list <- x.comp.list
            scales.tck <- x$x.scales$tck[1]
            rot <- as.numeric(x$x.scales$rot)[1]
            if (outside[1]) {
                ## use axis.default where possible (i.e. where outside=TRUE)
                ## because it handles labels well in multi-panel layouts
                x$x.scales$alternating <- 3
                x$axis(side = if (opposite[1]) "top" else "bottom",
                       scales = x$x.scales,
                       components = xscale.comps,
                       as.table = x$as.table,
                       rot = rot)
            } else {
                panel.axis(side = if (opposite[1]) "top" else "bottom",
                       at = comp.list$ticks$at,
                       labels = comp.list$labels$labels,
                                        #tick = do.ticks,
                                        #draw.labels = do.labels,
                       check.overlap = comp.list$labels$check.overlap,
                       outside = outside[1],
                       half = FALSE,
                       tck = scales.tck * comp.list$ticks$tck,
                       rot = rot)
            }
        }
        if (("y" %in% axes) && is.list(y.comp.list) && x$y.scales$draw) {
            comp.list <- y.comp.list
            scales.tck <- x$y.scales$tck[1]
            rot <- as.numeric(x$y.scales$rot)[1]
            if (outside[2]) {
                ## use axis.default where possible (i.e. where outside=TRUE)
                ## because it handles labels well in multi-panel layouts
                x$y.scales$alternating <- 3
                x$axis(side = if (opposite[2]) "right" else "left",
                       scales = x$y.scales,
                       components = yscale.comps,
                       as.table = x$as.table,
                       rot = rot)
            } else {
                panel.axis(side = if (opposite[2]) "right" else "left",
                       at = comp.list$ticks$at,
                       labels = comp.list$labels$labels,
                                        #tick = do.ticks,
                                        #draw.labels = do.labels,
                       check.overlap = comp.list$labels$check.overlap,
                       outside = outside[2],
                       half = FALSE,
                       tck = scales.tck * comp.list$ticks$tck,
                       rot = rot)
            }
        }
        upViewport(2)
    }, data = list(x = x, x.same = x.same, y.same = y.same,
       axes = axes, opposite = opposite, outside = outside),
          ...)
}
