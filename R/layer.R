##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

as.layer <- function(x, ...)
    UseMethod("as.layer")

as.layer.layer <- function(x, ...)
    x

layer <-
    function(..., data = NULL, eval = FALSE, etc = FALSE,
             packets = NULL,
             rows = NULL, columns = NULL, groups = NULL,
             under = FALSE, superpose = FALSE,
             style = NULL, theme = NULL)
{
    ## set layer to quoted expressions in `...`
    foo <- eval(substitute(expression(...)))
    if (eval) {
        for (i in seq_along(foo)) {
            icall <- foo[[i]]
            icall <- eval(call("substitute",
                               icall, list(.x = quote(quote(x)),
                                           .y = quote(quote(y)),
                                           .z = quote(quote(z)),
                                           .groups = quote(quote(groups)),
                                           .subscripts = quote(quote(subscripts)))))
            if (identical(etc, FALSE)) {
                icall[-1] <- lapply(icall[-1], eval.parent)
            } else {
                Args <- lapply(icall[-1], eval.parent)
                icall <-
                    substitute(do.call(.FUN,
                                       modifyList(list(...)[etc], Args)),
                               list(.FUN = icall[[1]],
                                    Args = Args,
                                    etc = etc))
            }
            foo[[i]] <- icall
        }
    }
    mostattributes(foo) <-
        list(data = data,
             under = under,
             packets = packets,
             rows = rows,
             columns = columns,
             groups = groups,
             superpose = superpose,
             style = style,
             theme = theme)
    lay <- list(foo)
    class(lay) <- c("layer", "trellis")
    lay
}

layer_ <- function(...)
{
    ccall <- match.call()
    ccall$under <- TRUE
    ccall[[1]] <- quote(layer)
    eval.parent(ccall)
}

glayer <- function(...)
{
    ccall <- match.call()
    ccall$superpose <- TRUE
    ccall[[1]] <- quote(layer)
    eval.parent(ccall)
}

glayer_ <- function(...)
{
    ccall <- match.call()
    ccall$superpose <- TRUE
    ccall$under <- TRUE
    ccall[[1]] <- quote(layer)
    eval.parent(ccall)
}

## to avoid print.trellis
print.layer <- print.default

## to avoid [.trellis and to keep the class attribute
"[.layer" <- function (x, i, ...)
    structure(unclass(x)[i], class = class(x))

"+.trellis" <- function(object, lay)
{
    ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(`+`)
    stopifnot(inherits(object, "trellis"))
    lay <- as.layer(lay)
    if (inherits(object, "layer")) {
        ## just concatenate lists
        return(structure(c(unclass(object), unclass(lay)),
                         class = c("layer", "trellis")))
    }
    panel <- if (toString(object$call[[1]]) == "splom")
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
        .UNDER <- unlist(lapply(lay, attr, "under"))
        ## underlayers only
        drawLayer(lay[.UNDER])
        ## original panel function:
        panel(..., subscripts = subscripts)
        ## overlayers only
        drawLayer(lay[.UNDER == FALSE])
    }
    object <- update(object, panel = newpanel)
    ## need this to allow further calls to update() to insert arguments:
    object$call <- call("update", ocall)
    object
}

drawLayer <- function(lay)
{
    lay <- as.layer(lay)
    .UNDER <- unlist(lapply(lay, attr, "under"))
    ## underlayers, in reverse order
    for (.ITEM in rev(lay[.UNDER]))
        drawLayerItem(.ITEM)
    ## overlayers
    for (.ITEM in lay[.UNDER == FALSE])
        drawLayerItem(.ITEM)
    invisible()
}

drawLayerItem <- function(layer.item)
{
    stopifnot(is.expression(layer.item))
    ## check that any restrictions on packets/rows/columns are met
    matchesok <- function(spec, value) {
        if (is.null(spec)) return(TRUE)
        if (all(spec <= 0))
            ## negative indexes exclude items
            return(value %in% -spec == FALSE)
        else
            return(value %in% spec)
    }
    matchesallok <-
        with(list(a = attributes(layer.item)),
             matchesok(a$packets, packet.number()) &&
             matchesok(a$rows, current.row()) &&
             matchesok(a$columns, current.column()))
    if (!matchesallok) return()
    ## set given theme for duration of this function
    if (!is.null(attr(layer.item, "theme"))) {
        .TRELLISPAR <- trellis.par.get()
        trellis.par.set(attr(layer.item, "theme"))
        on.exit(trellis.par.set(.TRELLISPAR))
    }
    ## define a layer drawing function, which may be per group
    drawLayerItemPerGroup <- function(...)
    {
        ## Note: layer.item is found in this function's environment
        dots <- list(...)
        ## restrict to specified group numbers
        if (!matchesok(attr(layer.item, "groups"), dots$group.number))
            return()
        if (!is.null(attr(layer.item, "style"))) {
            ## extract plot style attributes from given index into superpose.*
            .TRELLISPAR <- trellis.par.get()
            .STY <- attr(layer.item, "style")
            trellis.par.set(plot.line = Rows(trellis.par.get("superpose.line"), .STY),
                            add.line = Rows(trellis.par.get("superpose.line"), .STY),
                            add.text = Rows(trellis.par.get("superpose.line"), .STY),
                            plot.symbol = Rows(trellis.par.get("superpose.symbol"), .STY),
                            plot.polygon = Rows(trellis.par.get("superpose.polygon"), .STY),
                            axis.text = Rows(trellis.par.get("superpose.line"), .STY),
                            axis.line = Rows(trellis.par.get("superpose.line"), .STY)
                            )
            on.exit(trellis.par.set(.TRELLISPAR))
        }
        with(dots,
             eval(layer.item, attr(layer.item, "data"),
                  environment()))
    }
    ## call panel.superpose for group layers
    if (isTRUE(attr(layer.item, "superpose"))) {
        do.call("panel.superpose",
                c(trellis.panelArgs(),
                  list(panel.groups = drawLayerItemPerGroup)))
    } else {
        do.call("drawLayerItemPerGroup", trellis.panelArgs())
    }
}

flattenPanel <- function(object)
{
    flattenFun <- function(fun)
    {
        env <- environment(fun)
        ## check if this panel function is simple or has layers
        if (is.null(env) ||
            !exists(".is.a.layer", env, inherits = FALSE))
            return(as.expression(body(fun)))
        ## merge: under layers, existing panel, over layers
        .UNDER <- sapply(env$lay, attr, "under")
        c(do.call("c", rev(env$lay[.UNDER])),
          flattenFun(env$panel),
          do.call("c", env$lay[.UNDER == FALSE]))
    }
    flat <- flattenFun(object$panel)
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

