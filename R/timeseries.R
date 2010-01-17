

xyplot.ts <-
    function(x, data = NULL, type = 'l',
             auto.key = TRUE,
             cut = FALSE,
             default.scales = list(),
             ...)
    ## TODO: a way to specify styles for each series in 'x'? (by name?)
    ## TODO: and allow series in 'x' to be grouped into separate panels?
{
    stopifnot(is.null(data))
    ocall <- sys.call(sys.parent())
    data <- as.data.frame(x)
    nm <- names(data)
    unm <- make.names(c(nm, "Time"), unique = TRUE)
    names(data) <- unm[-length(unm)]
    tnm <- unm[length(unm)]
    data[[tnm]] <- as.vector(time(x))
    if (is.logical(cut) && cut) cut <- list()
    if (is.list(cut))
    {
        ecargs <- list(x = data[[tnm]])
        ecargs <- lattice:::updateList(ecargs, cut)
        data[[tnm]] <- do.call(equal.count, ecargs)
        ## tnm will work as numeric x-variable too
        form <-
            as.formula(sprintf("%s ~ %s | %s",
                               paste(lapply(unm[-length(unm)], as.name), collapse = "+"),
                               tnm, tnm))
        default.scales <-
            lattice:::updateList(list(x = list(relation = "sliced")),
                                 default.scales)
    }
    else
    {
        form <-
            as.formula(sprintf("%s ~ %s",
                               paste(lapply(unm[-length(unm)], as.name), collapse = "+"),
                               tnm))
    }
    if (is.logical(auto.key) && auto.key) auto.key <- list()
    if (is.list(auto.key))
        auto.key <-
            modifyList(list(lines = TRUE, points = FALSE),
                       auto.key)
    ans <- xyplot(form, data = data, type = type, auto.key = auto.key,
                  default.scales = default.scales, ...)
    ans$call <- ocall
    ans
}

llines.ts <-
    function(x, y = NULL, ...)
{
    if (!is.null(y)) {
        llines(as.vector(x), y = y, ...)
    } else {
        llines(as.vector(time(x)), y = as.vector(x), ...)
    }
}


xyplot.stl <-
    function(x, data = NULL,
             outer = TRUE,
             layout = c(1, 4),
             strip = FALSE,
             strip.left = TRUE,
             as.table = TRUE,
             ylab = "",
             between = list(y = 0.5),
             panel =
             function(..., type) {
                 if (packet.number() == 4) type <- "h"
                 panel.xyplot(..., type = type)
             },
             ...)
{
    stopifnot(is.null(data))
    mstrip <- missing(strip.left)
    sers <- x$time.series
    ## ncomp <- ncol(sers)
    data <- rowSums(sers)
    X <- cbind(data, sers)
    colnames(X) <- c("data", colnames(sers))
    ans <-
        xyplot(X,
               outer = outer,
               layout = layout,
               strip = strip,
               strip.left = strip.left,
               as.table = as.table,
               ylab = ylab,
               between = between,
               panel = panel,
               ...,
               default.scales =
               list(x = list(axs = "i"),
                    y =
                    list(relation = "free",
                         tick.number = 3,
                         rot = 0)))
    if (mstrip)
    {
        mx <- min(rx <- abs(sapply(ans$y.limits, diff)))
        int <- cbind(-mx / rx, mx / rx)
        ans <-
            update(ans,
                   strip.left =
                   strip.custom(horizontal = FALSE,
                                strip.names = FALSE,
                                strip.levels = TRUE,
                                shingle.intervals = int))
    }
    ans
}



