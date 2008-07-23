## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

is.categorical <- function (x)
{
    is.factor(x) || is.shingle(x) || is.character(x) || is.logical(x)
}

marginals <-
    function(data,
             reorder = TRUE,
             plot.points = FALSE,
             ref = TRUE,
             origin = 0,
             levels.fos = NULL,
             xlab = NULL, ylab = NULL,
             cex = 0.5,
             ...,
             subset = TRUE,
             as.table = TRUE,
             subscripts = TRUE,
             default.scales = list(
               x=list(relation="free", abbreviate=TRUE,
                 rot=60, cex=0.5, tick.number=3),
               y=list(relation="free", draw=FALSE)))
{
    if (!is.data.frame(data))
        data <- as.data.frame(data)
    nvar <- ncol(data)
    iscat <- sapply(data, is.categorical)
    ## apply subset
    ## evaluate in context of data, or if that fails just eval as usual
    ## (because latticist uses: subset=complete.cases(dat))
    tmp <- try(eval(substitute(subset), data), silent=TRUE)
    if (!inherits(tmp, "try-error")) subset <- tmp
    if (!isTRUE(subset)) data <- data[subset,]
    ## reorder factor levels
    if (reorder) {
        for (nm in names(data)[iscat]) {
            val <- data[[nm]]
            if (is.character(val))
                data[[nm]] <- factor(val)
            if (!is.ordered(val) &&
                !is.shingle(val) &&
                nlevels(val) > 1)
            {
                data[[nm]] <- reorder(val, val, function(z) -length(z))
            }
        }
    }
    if (any(iscat)) {
        facdat <- lapply(data[iscat], function(Value)
                         as.data.frame(table(Value)) )
        facdat <- do.call(make.groups, facdat)
        ## order packets by number of levels, same effect as index.cond
        facdat$which <- with(facdat, reorder(which, which, length))
        ## make trellis object for factors
        factobj <-
            dotplot(Freq ~ Value | which, data=facdat, subscripts=TRUE,
                    ...,
                    type=c("p","h"), cex=cex,
                    levels.fos = levels.fos,
                    origin = origin,
                    as.table = as.table,
                    default.scales = default.scales,
                    xlab=xlab, ylab=ylab)
        if (all(iscat)) return(factobj)
    }
    if (any(!iscat)) {
        numdat <- do.call(make.groups, data[!iscat])
        ## order packets by mean, same effect as index.cond
        numdat$which <- with(numdat, reorder(which, data, mean, na.rm=TRUE))
        ## make trellis object for numerics
        numobj <-
            densityplot(~ data | which, data=numdat, subscripts=TRUE,
                        ...,
                        plot.points=plot.points, ref=ref,
                        as.table = as.table,
                        default.scales = default.scales,
                        xlab=xlab, ylab=ylab)
        if (all(!iscat)) return(numobj)
    }
    ## if there is a mixture of categorical and numerics,
    ## merge the trellis objects
    return(c(factobj, numobj))
}

