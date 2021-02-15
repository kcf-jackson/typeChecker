#' Parse and annotate an expression
#'
#' @param x A parsed tree; the output from `parse`.
#
# @examples
# input <- parse(text = "a <- f(g(x), g(x))")
# x <- annotate_expr(input)[[1]]
# x
# invisible(Map(print, x))
# invisible(Map(print, x[[3]]))
annotate_expr <- function(x) {
    # Note 1: 'parse_table' is protected by lexical scoping; could
    # use an environment instead if desired.
    parse_table <- getParseTable(x)

    add_annotation <- function(expr) {
        # Since attributes cannot be set on a symbol, only calls have
        # location information attached to them.
        if (rlang::is_call(expr)) {
            expr <- as.call(purrr::map(expr, add_annotation))
            attributes(expr) <- get_location(expr)
        }
        return(expr)
    }

    get_location <- function(expr) {
        x <- deparse2(expr)
        df0 <- parse_table
        ind <- min(which(df0$text2 == x & !df0$used))

        parse_table$used[ind] <<- TRUE  # See Note 1
        return(df0[ind, ])
    }

    purrr::map(x, add_annotation)
}

getParseTable <- function(x) {
    res <- getParseData(x, includeText = TRUE)
    res$used <- FALSE  # for tracking the same expression appearing at different locations

    # The following lines of code are needed because
    # > deparse(parse(text="a ? b")[[1]])
    # returns "`?`(a, b)", which is not idempotent.
    deparse_safe_parse <- function(x) {
        res <- purrr::safely(parse)(text = x, keep.source = FALSE)
        if (!is.null(res$error) || length(res$result) == 0) return(x)
        return(deparse2(res$result[[1]]))
    }
    res$text2 <- purrr::map_chr(res$text, deparse_safe_parse)

    return(res)
}

deparse2 <- function(x) {
    paste(deparse(x, width.cutoff = 500L), collapse = "\n")
}
