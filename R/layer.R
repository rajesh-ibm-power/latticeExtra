
## Copyright (C) 2008 Felix Andrews <felix@nfrac.org>
## GPL >= 2

layer <- function(..., data = NULL, under = FALSE, layer = NULL)
{
    foo <- match.call()
    ## set layer to quoted expressions in `...`
    foo$layer <- NULL
    foo$data <- NULL
    foo$under <- NULL
    foo <- as.expression(as.list(foo)[-1])
    ## if 'layer' specified, wrap some code around it
    if (is.numeric(layer) && (layer > 0)) {
        setlayer <- substitute({
            .TRELLISPAR <- trellis.par.get()
            trellis.par.set(plot.line = Rows(trellis.par.get("superpose.line"), N),
                            add.line = Rows(trellis.par.get("superpose.line"), N),
                            plot.symbol = Rows(trellis.par.get("superpose.symbol"), N),
                            plot.polygon = Rows(trellis.par.get("superpose.polygon"), N))
            }, list(N=layer))
        foo <- c(setlayer, foo,
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

"+.trellis" <- function(x, lay)
{
    e1 <- x
    e2 <- lay

    e1.layer <- (inherits(e1, "layer"))
    e2.layer <- (inherits(e2, "layer"))
    if (e1.layer && e2.layer) {
        ## just concatenate lists
        return(structure(c(e1, e2)),
               class=c("layer", "trellis"))
    }
    if (!e1.layer && !e2.layer) {
        stop("'+.trellis' only works with layer objects")
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
    newpanel <- function(...) {
        dots <- list(...)
        with(dots, for (foo in rev(layer))
             if (attr(foo, "under") == TRUE)
             eval(foo, attr(foo, "data"),
                  environment()))
        panel(...)
        with(dots, for (foo in layer)
             if (attr(foo, "under") == FALSE)
             eval(foo, attr(foo, "data"),
                  environment()))
    }
    update(object, panel=newpanel)
}

layer1 <- function(...) {
    ccall <- match.call()
    ccall$layer <- 1
    ccall[[1]] <- quote(layer)
    eval.parent(ccall)
}

layer2 <- function(...) {
    ccall <- match.call()
    ccall$layer <- 2
    ccall[[1]] <- quote(layer)
    eval.parent(ccall)
}

layer3 <- function(...) {
    ccall <- match.call()
    ccall$layer <- 3
    ccall[[1]] <- quote(layer)
    eval.parent(ccall)
}

layer4 <- function(...) {
    ccall <- match.call()
    ccall$layer <- 4
    ccall[[1]] <- quote(layer)
    eval.parent(ccall)
}

layer5 <- function(...) {
    ccall <- match.call()
    ccall$layer <- 5
    ccall[[1]] <- quote(layer)
    eval.parent(ccall)
}

undoLayer <- function(x)
{
    stopifnot(is.function(x$panel))
    env <- environment(x$panel)
    if (!exists(".is.a.layer", env, inherits=FALSE))
        stop("does not look like a layer")
    update(x, panel=env$panel)
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

## TODO: needs documentation
panel.refline <-
    function(...,
             col.line = ref.line$col,
             lty = ref.line$lty,
             lwd = ref.line$lwd,
             alpha = ref.line$alpha)
{
    ref.line <- trellis.par.get("reference.line")
    panel.abline(..., col.line=col.line, lty=lty,
                 lwd=lwd, alpha=alpha)
}
