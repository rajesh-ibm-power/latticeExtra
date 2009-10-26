##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
##


panel.xblocks <- function(x, ...)
    UseMethod("panel.xblocks")

panel.xblocks.default <-
    function (x, y, ..., height = unit(1, "npc"),
              block.y = unit(0, "npc"), vjust = 0,
              col = NULL, border = NA, name = "xblocks",
              last.step = median(diff(tail(x))))
{
    x <- as.numeric(x)
    if (length(x) == 0) return()
    if (is.unsorted(x, na.rm = TRUE))
        stop("'x' should be ordered (increasing)")
    if (is.na(last.step))
        last.step <- 0
    ## this will convert factor to character:
    y <- as.vector(y)
    if (is.null(col)) {
        ## in general case, assume that 'y' gives the block colours.
        ## rle treats each NA as unique (does not combine runs of NAs)
        ## so we will replace NAs with equivalent transparent colour.
        if (is.character(y)) {
            y[is.na(y)] <- "transparent"
        } else {
            y[is.na(y)] <- 0
        }
    } else {
        ## a single colour was specified for blocks,
        ## so just take NA / FALSE values as transparent
        if (is.logical(y)) {
            y <- !is.na(y) & y
        } else {
            y <- !is.na(y)
        }
        y <- ifelse(y, col, if (is.character(col)) "transparent" else 0)
    }
    yrle <- rle(y)
    idxStart <- cumsum(c(1, yrle$lengths))
    idxStart <- head(idxStart, -1)
    idxEnd <- idxStart[-1]
    blockStart <- x[idxStart]
    blockEnd <- x[idxEnd]
    blockEnd <- c(blockEnd, tail(blockStart, 1) + last.step)
    blockWidth <- blockEnd - blockStart
    blockCol <- yrle$values
    ## draw it
    grid::grid.rect(x = blockStart, width = blockWidth,
                    y = block.y, height = height,
                    hjust = 0, vjust = vjust,
                    default.units = "native", name = name,
                    gp = gpar(fill = blockCol, col = border, ...))
}

panel.xblocks.ts <- function(x, y = NULL, ...)
{
    if (!is.null(y)) {
        panel.xblocks.default(x, y, ...)
    } else {
        panel.xblocks.default(as.vector(time(x)), as.vector(x), ...)
    }
}

#panel.xblocks.zoo <- function (x, ...)
#{
#    x <- as.zoo(x)
#    panel.xblocks.default(time(x), coredata(x), ...)
#}
