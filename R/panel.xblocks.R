##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
##


panel.xblocks <- function(x, ...)
    UseMethod("panel.xblocks")

panel.xblocks.default <-
    function (x, y, ..., gaps = FALSE,
              height = unit(1, "npc"),
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
    ## gaps: can't just call is.na() on the input because
    ## zoo and ts objects lose their time attributes.
    if (gaps)
        y <- is.na(y)
    ## Three cases:
    ## (1) If y is character, assume it gives the block colours
    ## -- unless 'col' is given, which over-rides it.
    ## (2) If y is logical, show blocks of TRUE values.
    ## (3) If y is numeric, show blocks of non-NA values.
    if (mode(y) == "numeric") ## includes Date etc
        y <- !is.na(y)
    ## Note: rle treats each NA as unique (does not combine runs of NAs)
    ## so we need to replace NAs with a temporary value.
    NAval <-
        if (is.character(y)) "" else FALSE
    y[is.na(y)] <- NAval
    ## find blocks (runs of constant values)
    yrle <- rle(y)
    ## substitute NA values back in
    blockCol <- yrle$values
    blockCol[blockCol == NAval] <- NA
    ## for logical series, col default comes from current theme
    if (is.logical(y) && is.null(col))
        col <- trellis.par.get("plot.line")$col
    ## set block colours if 'col' given
    if (length(col) > 0) {
        ok <- !is.na(blockCol)
        blockCol[ok] <- rep(col, length = sum(ok)) ## rep to avoid warnings
    }
    ## work out block geometry
    idxBounds <- cumsum(c(1, yrle$lengths))
    idxStart <- head(idxBounds, -1)
    idxEnd <- tail(idxBounds, -1)
    idxEnd[length(idxEnd)] <- length(y)
    blockStart <- x[idxStart]
    blockEnd <- x[idxEnd]
    blockEnd[length(blockEnd)] <- tail(blockEnd, 1) + last.step
    blockWidth <- blockEnd - blockStart
    ## draw it
    grid::grid.rect(x = blockStart, width = blockWidth,
                    y = block.y, height = height,
                    hjust = 0, vjust = vjust,
                    default.units = "native", name = name,
                    gp = gpar(fill = blockCol, col = border, ...))
}

panel.xblocks.zoo <-
panel.xblocks.ts <-
    function(x, y = NULL, ...)
{
    if (!is.null(y)) {
        panel.xblocks.default(x, y, ...)
    } else {
        panel.xblocks.default(as.vector(time(x)), as.vector(x), ...)
    }
}
