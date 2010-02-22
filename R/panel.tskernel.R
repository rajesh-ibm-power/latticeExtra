
simpleSmoothTs <- 
    function(x, width = NROW(x) %/% 10 + 1,
             c = 1, sides = 2, circular = FALSE,
             kern = kernel("daniell", rep(floor((width/sides)/sqrt(c)), c)))
{
    if (sides == 2) {
        ii <- -kern$m:kern$m
        filter <- kern[ii]
    } else if (sides == 1) {
        ii <- -kern$m:0
        filter <- kern[ii] / sum(kern[ii]) ## normalise
    } else stop("unrecognised value of 'sides'")
    xf <- x
    xf[] <- filter(as.matrix(x), filter, sides = sides, circular = circular)
    xf
}

panel.tskernel <-
    function(x, y, ...,
             width = NROW(x) %/% 10 + 1,
             c = 1, sides = 2, circular = FALSE,
             kern = kernel("daniell", rep(floor((width/sides)/sqrt(c)), c)))
{
    if (!missing(y)) {
        if (diff(range(diff(x))) > getOption("ts.eps"))
            stop("'x' should be a regular series")
        x <- ts(y, start = x[1], end = tail(x,1), deltat = diff(x[1:2]))
    }
    x <- as.ts(x)
    panel.lines(simpleSmoothTs(x, width = width, c = c, sides = sides,
                               circular = circular, kern = kern),
                ...)
}
