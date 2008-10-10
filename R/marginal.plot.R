##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

is.categorical <- function (x)
{
    is.factor(x) || is.shingle(x) || is.character(x) || is.logical(x)
}

marginal.plot <-
    function(x,
             data = NULL,
             groups = NULL,
             reorder = TRUE,
             plot.points = FALSE,
             ref = TRUE,
             origin = 0,
             xlab = NULL, ylab = NULL,
             type = c("p", if (is.null(groups)) "h"),
             ...,
             subset = TRUE,
             as.table = TRUE,
             subscripts = TRUE,
             par.settings = simpleTheme(cex = 0.6),
             default.scales = list(
               x = list(relation = "free", abbreviate = TRUE,
                 rot = 60, cex = 0.5, tick.number = 3),
               y = list(relation = "free", draw = FALSE)))
{
    ## assume first term of formula is the data object; ignore rest
    if (inherits(x, "formula"))
        x <- eval(x[[2]], data, environment(x))
    if (!is.data.frame(x))
        x <- as.data.frame(x)
    ## groups and subset are subject to non-standard evaluation:
    groups <- eval(substitute(groups), data, parent.frame())
    ## note unusual cases e.g.
    ## evalq(marginal.plot(dat, subset = complete.cases(dat)), myEnv)
    subset <- eval(substitute(subset), data, parent.frame())
    ## apply subset
    if (!isTRUE(subset)) x <- x[subset,]
    ## divide into categoricals and numerics
    iscat <- sapply(x, is.categorical)
    ## reorder factor levels
    if (reorder) {
        for (nm in names(x)[iscat]) {
            val <- x[[nm]]
            if (is.character(val))
                x[[nm]] <- factor(val)
            if (!is.ordered(val) &&
                !is.shingle(val) &&
                nlevels(val) > 1)
            {
                x[[nm]] <- reorder(val, val, function(z) -length(z))
            }
        }
    }
    if (any(iscat)) {
        ## handle categorical variables
        ## make a list of dotplot trellis objects
        dotobjs <-
            lapply(x[iscat], function(x)
               {
                   if (!is.null(groups)) {
                       tab <- table(Value = x, groups = groups)
                   } else {
                       tab <- table(Value = x)
                   }
                   dotplot(tab, horizontal = FALSE,
                           groups = !is.null(groups),
                           subscripts = TRUE,
                           ...,
                           type = type,
                           origin = origin,
                           as.table = as.table,
                           par.settings = par.settings,
                           default.scales = default.scales,
                           xlab = xlab, ylab = ylab)
               })
        ## merge the list of trellis objects into one
        catobj <- do.call("c", dotobjs)
        catobj$call <- match.call()
        if (all(iscat)) return(catobj)
    }
    if (any(!iscat)) {
        ## handle numeric variables
        ## construct formula with all numeric variables
        numform <- paste("~", paste(names(x)[!iscat],
                                    collapse = " + "))
        numobj <-
            densityplot(as.formula(numform), x, outer = TRUE,
                        subscripts = TRUE,
                        groups = groups,
                        ...,
                        plot.points = plot.points, ref = ref,
                        as.table = as.table,
                        par.settings = par.settings,
                        default.scales = default.scales,
                        xlab = xlab, ylab = ylab)
        ## set strip name if only one panel
        if (prod(dim(numobj)) == 1)
            rownames(numobj) <- names(x)[!iscat]
        numobj$call <- match.call()
        if (all(!iscat)) return(numobj)
    }
    ## if there are both categoricals and numerics,
    ## merge the trellis objects; keep original var order
    reIndex <- order(c(which(iscat), which(!iscat)))
    obj <- update(c(catobj, numobj), index.cond = list(reIndex))
    ## force strips when only one panel in each object
    if (identical(obj$strip, FALSE))
        obj$strip <- "strip.default"
    obj$call <- match.call()
    obj
}
