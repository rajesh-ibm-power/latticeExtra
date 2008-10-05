##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

layer <- function(..., data = NULL, under = FALSE, style = NULL)
{
    foo <- match.call()
    ## set layer to quoted expressions in `...`
    foo$style <- NULL
    foo$data <- NULL
    foo$under <- NULL
    foo <- as.expression(as.list(foo)[-1])
    ## if 'style' specified, wrap some code around it
    if (is.numeric(style) && (style > 0)) {
        setstyle <- substitute({
            .TRELLISPAR <- trellis.par.get()
            trellis.par.set(plot.line = Rows(trellis.par.get("superpose.line"), N),
                            add.line = Rows(trellis.par.get("superpose.line"), N),
                            add.text = Rows(trellis.par.get("superpose.line"), N),
                            plot.symbol = Rows(trellis.par.get("superpose.symbol"), N),
                            plot.polygon = Rows(trellis.par.get("superpose.polygon"), N),
                            axis.text = Rows(trellis.par.get("superpose.line"), N),
                            axis.line = Rows(trellis.par.get("superpose.line"), N)
                            )
            }, list(N = style))
        foo <- c(setstyle, foo,
                 quote(trellis.par.set(.TRELLISPAR)))
    }
    attr(foo, "data") <- data
    attr(foo, "under") <- under
    lay <- list(foo)
    class(lay) <- c("layer", "trellis")
    lay
}

## to avoid print.trellis
print.layer <- print.default

as.layer <- function(x, ...) UseMethod("as.layer")

as.layer.trellis <-
    function(x,
             axes = c("x", "y"),
             opposite = TRUE,
             outside = FALSE,
             ...)
{
    if (identical(axes, TRUE)) axes <- c("x", "y")
    if (identical(axes, FALSE)) axes <- NULL
    opposite <- rep(opposite, length = 2)
    outside <- rep(outside, length = 2)
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
    }, data = list(x = x, axes = axes,
       opposite = opposite, outside = outside),
          ...)
}

"+.trellis" <- function(x, lay)
{
    e1 <- x
    e2 <- lay
    e1.layer <- (inherits(e1, "layer"))
    e2.layer <- (inherits(e2, "layer"))
    if (!e1.layer && !e2.layer) {
        stop("'+.trellis' only works with layer objects")
    }
    if (e1.layer && e2.layer) {
        ## just concatenate lists
        return(structure(c(e1, e2)),
               class=c("layer", "trellis"))
    }
    object <- if (e1.layer) e2 else e1
    layer <- if (e2.layer) e2 else e1
    ## get rid of "trellis" class, it interferes with eg "["
    class(layer) <- "layer"
    for (i in seq_along(layer)) {
        if (isTRUE(attr(layer[[i]], "data"))) {
            ## try to get 'data' argument from lattice call
            fullcall <- match.call(eval(object$call[[1]]), object$call)
            if ("data" %in% names(fullcall)) {
                data <- eval.parent(fullcall$data) ## may fail
                attr(layer[[i]], "data") <- data
            } else stop("no 'data' argument in original call")
        }
    }
    panel <- if (as.character(object$call[[1]]) == "splom")
        object$panel.args.common$panel
    else object$panel
    panel <- if (is.function(panel)) panel
    else if (is.character(panel)) {
        ## could be just get(panel), but for flattenPanel:
        ## do not expand original panel function eg panel.xyplot(...)
        tmp <- function(...) NA
        body(tmp) <- call(panel, quote(...))
        environment(tmp) <- globalenv()
        tmp
    } else eval(panel)
    ## a flag to indicate this panel function has layers
    ## (used by flattenPanel and undoLayer)
    .is.a.layer <- TRUE
    newpanel <- function(..., subscripts = TRUE) {
        dots <- list(...)
        with(dots, for (foo in rev(layer))
             if (attr(foo, "under") == TRUE)
             eval(foo, attr(foo, "data"),
                  environment()))
        panel(..., subscripts = subscripts)
        with(dots, for (foo in layer)
             if (attr(foo, "under") == FALSE)
             eval(foo, attr(foo, "data"),
                  environment()))
    }
    update(object, panel=newpanel)
}

flattenPanel <- function(x)
{
    flattenFun <- function(fun)
    {
        env <- environment(fun)
        ## check if this panel function is simple or has layers
        if (is.null(env) ||
            !exists(".is.a.layer", env, inherits=FALSE))
            return(as.expression(body(fun)))
        ## merge: under layers, existing panel, over layers
        underlays <- sapply(env$layer, attr, "under")
        c(do.call("c", rev(env$layer[underlays])),
          flattenFun(env$panel),
          do.call("c", env$layer[!underlays]))
    }
    flat <- flattenFun(x$panel)
    ## wrap in braces, as in a function body
    as.call(c(quote(`{`), flat))
}

## not exported -- I do not think this is really useful
undoLayer <- function(x)
{
    stopifnot(is.function(x$panel))
    env <- environment(x$panel)
    if (!exists(".is.a.layer", env, inherits=FALSE))
        stop("does not look like a layer")
    update(x, panel=env$panel)
}

