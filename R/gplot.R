



### EXPERIMENTAL: a general 'grouped display' function gplot that
### could potentially be used to display any R object using S3 method
### dispatch


## Making gplot generic, but we only plan to use the default method,
## making use of the gplotArgs generic

gplot <- function(x, ...)
    UseMethod("gplot")


## The rest concerns gplot.default


## The idea of the gplot function is as follows: It can be thought of
## as a new plot method, typically to be applied to data frames.  It
## would eventually call a lattice function, but there would be a few
## extra arguments that would be pre-processed.  These are modeled on
## the groupedData() constructor in nlme.

## to keep things conceptually as simple as possible, here's what
## would happen with a call of the form gplot(x, ...):

## 1 start with list: callList <- list(data = x)
## 2 update callList with gplotArgs(x, ...)
## 3 callList MUST now have a component called plotFun.  Do a
##   do.call(plotFun, <rest of callList>)

## In practice, the only non-trivial stuff will be done by
## gplotArgs().  For now, we will only have a gplotArgs method for
## data frames.  For objects without specific methods, the default
## will be to use the "gplot.args" attribute





gplotArgs <- function(x, ...)
    UseMethod("gplotArgs")


## not very useful.  More methods defined below.

gplotArgs.default <- 
    function(x, ...)
{
    updateList(attr(x, "gplot.args"), list(...))
}



gplot.default <- function(x, ...)
{
    callList <- updateList(list(data = x), gplotArgs(x, ...))
    plotFun <- callList$plotFun
    if (!is.null(callList$display.formula))
    {
        callList$x <- callList$display.formula
        ## callList$formula <- callList$display.formula
        callList$display.formula <- NULL
    }
    if (is.null(plotFun))
        stop("display function not specified")
    else
    {
        callList$plotFun <- NULL
        do.call(plotFun, callList)
    }
}


## generic to set attributes for gplot.  Normally everything should be
## put in the "gplot.args" attribute, but methods are allowed to
## override it and handle some arguments specially.


"gplotArgs<-" <- function(x, value)
    UseMethod("gplotArgs<-")


"gplotArgs<-.default" <- 
    function(x, value)
{
    if (!is.list(value)) stop("assigned value must be list")
    attr(x, "gplot.args") <- value
    x
}



