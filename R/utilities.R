
## update elements of a list recursively. 


updateList <-
    function(x, val)
{
    if (is.null(x)) x <- list()
    if (!is.list(x)) stop("x must be NULL or a list")
    if (!is.list(val)) stop("val must be a list")
    xnames <- names(x)
    for (v in names(val))
    {
        existing <- v %in% xnames
        if (existing && is.list(x[[v]]) && is.list(val[[v]]))
            x[[v]] <- updateList(x[[v]], val[[v]])
        else 
            x[[v]] <- val[[v]]
    }
    x
}






## utility functions to extract components of a formula.  Don't work
## reliably with unusual symbols

.responseName <- function(formula)
{
    if (length(formula) == 3) as.character(formula[2])
    else stop("invalid formula")
}

.covariateName <- function(formula)
{
    RHS <- 
        if (length(formula) == 3) as.character(formula[3])
        else if (length(formula) == 2) as.character(formula[2])
        else stop("invalid formula")
    RHS <- strsplit(RHS, " | ", fixed = TRUE)[[1]]
    RHS[1]
}

.groupsName <- function(formula)
{
    RHS <- 
        if (length(formula) == 3) as.character(formula[3])
        else if (length(formula) == 2) as.character(formula[2])
        else stop("invalid formula")
    RHS <- strsplit(RHS, " | ", fixed = TRUE)[[1]]
    RHS[2]
}


