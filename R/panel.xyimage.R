
## Similar to panel.xyplot, except that (x,y) data are indicated by an
## image rather than standard plotting character. The image URLs must
## be provided by 


## Support only png and jpeg for now.

url2raster <- function(src)
{
    ext <- tail(strsplit(src, split = ".", fixed = TRUE)[[1]], 1)
    if (file.exists(src)) file <- src
    else 
    {
        file <- tempfile(fileext = paste0(".", ext))
        download.file(src, destfile = file, mode = "wb", quiet = TRUE)
        on.exit(unlink(file))
    }
    readWith <- switch(ext,
                       png = list(readPNG, readJPEG),
                       list(readJPEG, readPNG))
    ## Try best guess first. If it fails, try the other
    r <- try(readWith[[1]](file, native = TRUE), silent = TRUE)
    if (inherits(r, "try-error"))
        r <- try(readWith[[2]](file, native = TRUE), silent = TRUE)
    if (inherits(r, "try-error"))
        stop("'%s' does not appear to be a PNG or JPEG file.", src)
    r
}


panel.xyimage <-
    function(x, y, 
             subscripts,
             groups = NULL,
             pch = NULL,
             cex = 1,
             ...,
             grid = FALSE, abline = NULL)
{
    if (all(is.na(x) | is.na(y))) return()
    if (!is.character(pch))
        stop("'pch' must be a character vector giving path(s) or URL(s) of PNG or JPEG files.")
    if (!identical(grid, FALSE))
    {
        if (!is.list(grid))
            grid <- switch(as.character(grid),
                           "TRUE" = list(h = -1, v = -1, x = x, y = y),
                           "h" = list(h = -1, v = 0, y = y),
                           "v" = list(h = 0, v = -1, x = x),
                           list(h = 0, v = 0))
        do.call(panel.grid, grid)
    }
    if (!is.null(abline))
    {
        if (is.numeric(abline)) abline <- as.list(abline)
        do.call(panel.abline, abline)
    }
    if (is.null(groups))
    {
        pch.raster <- url2raster(pch[1])
        grid.raster(x, y, image = pch.raster,
                    width = unit(cex * 10, "mm"),
                    height = unit(cex * 10, "mm"),
                    default.units = "native")
    }
    else
    {
        groups <- as.factor(groups)
        cex <- rep(cex, length = nlevels(groups))
        pch <- rep(pch, length = nlevels(groups))
        pch.raster <- lapply(pch, url2raster)
        g <- as.numeric(groups)[subscripts]
        ug <- unique(g)
        for (i in ug)
        {
            w <- (g == i)
            grid.raster(x[w], y[w], image = pch.raster[[i]],
                        width = unit(cex[i] * 8, "mm"),
                        height = unit(cex[i] * 8, "mm"),
                        default.units = "native")
        }
    }
}


