##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

c.trellis <- function(..., x.same = FALSE, y.same = FALSE,
                      recursive = FALSE)
{
    objs <- list(...)
    if (length(objs) == 0) return(NULL)
    if (length(objs) == 1) {
        ## only one object
        obj <- objs[[1]]
        ## set dimnames if given and only one panel
        if (!is.null(names(objs)) && (prod(dim(obj)) == 1))
            rownames(obj) <- names(objs)
        return(obj)
    }
    if (length(objs) > 2) {
        ## merge first two objects, and call again
        objs <- c(list(do.call("c", objs[1:2])),
                       objs[-(1:2)])
        return(do.call("c", objs))
    }
    ## now exactly 2 objects
    obj1 <- objs[[1]]
    obj2 <- objs[[2]]
    ## number of packets in object, i.e. offset
    NPACK1 <- prod(dim(obj1))
    NPACK2 <- prod(dim(obj2))
    ## first panel function
    panel <- obj1$panel
    PANEL1 <- if (is.function(panel)) panel
    else if (is.character(panel)) get(panel)
    else eval(panel)
    ## second panel function
    panel <- obj2$panel
    PANEL2 <- if (is.function(panel)) panel
    else if (is.character(panel)) get(panel)
    else eval(panel)
    obj1$panel <- function(...) {
        if (packet.number() <= NPACK1)
            PANEL1(...)
        else PANEL2(...)
    }
    ## flatten the trellis objects (make 1 dimensional)
    flatIC <- function(index.cond) {
        dim <- sapply(index.cond, length)
        ic <- do.call(expand.grid, index.cond)
        if (length(dim) >= 2)
            ic[,2] <- (ic[,2] - 1) * dim[1]
        if (length(dim) >= 3)
            ic[,3] <- (ic[,3] - 1) * prod(dim[1:2])
        rowSums(ic)
    }
    flatCL <- function(condlevels, newname=NULL) {
        ## paste names of variables to their values (for strips)
        #for (i in seq_along(condlevels))
        #    condlevels[[i]] <- paste(names(condlevels)[i], ## may be NULL
        #                             condlevels[[i]], sep=" = ")
        ## convert shingle levels to character strings
        condlevels <- lapply(condlevels, as.character)
        cl <- do.call(expand.grid, condlevels)
        cl <- apply(cl, 1, paste, sep=" / ")
        if (!is.null(newname) && (nchar(newname) > 0)) {
            if (length(cl) == 1) cl <- newname
            else cl <- paste(newname, cl, sep=": ")
        }
        cl
    }
    obj1$index.cond <- list(c(flatIC(obj1$index.cond),
                              flatIC(obj2$index.cond) + NPACK1))
    obj1$condlevels <- list(c(flatCL(obj1$condlevels, names(objs)[1]),
                              flatCL(obj2$condlevels, names(objs)[2])))
    obj1$perm.cond <- 1
    ## make scales nominally "free", so they look like original objects
    makeFreeScales <- function(obj, npack, x.y)
    {
        obj[[paste(x.y, "scales", sep=".")]]$relation <- "free"
        .limits <- paste(x.y, "limits", sep=".")
        .num.limit <- paste(x.y, "num.limit", sep=".")
        .used.at <- paste(x.y, "used.at", sep=".")
        if (is.null(obj[[.limits]])) obj[[.limits]] <- NA
        if (is.null(obj[[.num.limit]])) obj[[.num.limit]] <- NA
        if (is.null(obj[[.used.at]])) obj[[.used.at]] <- NA
        if (!is.list(obj[[.limits]])) {
            obj[[.limits]] <- rep(list(obj[[.limits]]), length=npack)
            obj[[.num.limit]] <- rep(list(obj[[.num.limit]]), length=npack)
            obj[[.used.at]] <- rep(list(obj[[.used.at]]), length=npack)
        }
        obj
    }
    ## set relations to "free" if the first object has "free" scales
    ## or if the limits in the two objects are not identical
    xlimItems <- c("x.limits", "x.num.limit", "x.used.at")
    ylimItems <- c("y.limits", "y.num.limit", "y.used.at")
    if (missing(x.same)) {
        x.same <- FALSE
        if (!is.list(obj1$x.limits) &&
            identical(unclass(obj1)[xlimItems],
                      unclass(obj2)[xlimItems]))
            x.same <- TRUE
    }
    if (missing(y.same)) {
        y.same <- FALSE
        if (!is.list(obj1$y.limits) &&
            identical(unclass(obj1)[ylimItems],
                      unclass(obj2)[ylimItems]))
            y.same <- TRUE
    }
    if ((x.same == FALSE) ||
        is.list(obj1$x.limits))
    {
        obj1 <- makeFreeScales(obj1, npack=NPACK1, x.y="x")
        obj2 <- makeFreeScales(obj2, npack=NPACK2, x.y="x")
        obj1$x.limits <- c(obj1$x.limits, obj2$x.limits)
        obj1$x.num.limit <- c(obj1$x.num.limit, obj2$x.num.limit)
        obj1$x.used.at <- c(obj1$x.used.at, obj2$x.used.at)
    }
    if ((y.same == FALSE) ||
        is.list(obj1$y.limits))
    {
        obj1 <- makeFreeScales(obj1, npack=NPACK1, x.y="y")
        obj2 <- makeFreeScales(obj2, npack=NPACK2, x.y="y")
        obj1$y.limits <- c(obj1$y.limits, obj2$y.limits)
        obj1$y.num.limit <- c(obj1$y.num.limit, obj2$y.num.limit)
        obj1$y.used.at <- c(obj1$y.used.at, obj2$y.used.at)
    }
    ## merge common panel args into panel.args
    ## TODO: check for identical() args
    obj1$panel.args <- lapply(obj1$panel.args, c,
                              obj1$panel.args.common)
    obj2$panel.args <- lapply(obj2$panel.args, c,
                              obj2$panel.args.common)
    obj1$panel.args.common <- list()
    ## the actual data
    obj1$panel.args <- c(obj1$panel.args, obj2$panel.args)
    obj1$packet.sizes <- c(obj1$packet.sizes, obj2$packet.sizes)
    ## turn strips on if either object has strips, or names given
    if (identical(obj1$strip, FALSE) &&
        !identical(obj2$strip, FALSE))
        obj1$strip <- obj2$strip
    if (identical(obj1$strip, FALSE) &&
        !is.null(names(objs)))
        obj1$strip <- "strip.default"
    obj1$call <- call("c", obj1$call, obj2$call)
    obj1
}
