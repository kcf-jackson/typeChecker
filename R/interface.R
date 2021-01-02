interpreter <- function() {
    quit <- FALSE
    envir <- list()
    while (!quit) {
        x <- readline("type-mode > ")
        if (x == "q()") quit <- TRUE
        res <- eval_type(rlang::parse_expr(x), envir)
        envir <- res$envir
        print(envir)
    }
    envir
}


#' @param x An XML tree returned by `xml2::as_list(xml2::read_xml())`
get_line_col <- function(x) {
    if (!is.list(x[[1]])) {
        return(list(
            line_number = as.integer(attr(x, "line1")),
            column_number = as.integer(attr(x, "col1"))
        ))
    } else {
        return(Recall(x[[1]]))
    }
}


#' Type check an R file
#'
#' @param path A character string; the file path.
#'
#' @examples
#' file <- system.file("example/test_1.R", package = "typeChecker")
#' cat(paste(readLines(file), collapse = "\n"))
#' type_check(file)
#'
#' @export
type_check <- function(text) {
    es <- parse(text = text, keep.source = TRUE)
    tree <- xml2::as_list(xml2::read_xml(xmlparsedata::xml_parse_data(es)))[[1]]
    envir <- list()
    safe_eval_type <- purrr::safely(eval_type)
    errors <- list()
    for (i_expr in seq_along(es)) {
        expr <- es[[i_expr]]
        res <- safe_eval_type(expr, envir)
        if (!is.null(res$error)) {
            line_col <- get_line_col(tree[[i_expr]])
            cat("In the expression:\n")
            cat("The following type error is found:\n")
            cat(res$error$message)
            errors <- c(errors, list(list(
                filename = "<text>",
                line_number = line_col$line_number,
                column_number = line_col$column_number,
                type = "type",
                message = res$error$message,
                line = expr,
                ranges = NULL,
                linter = "typeChecker"
            )))
        } else {
            envir <- res$result$envir
        }
    }
    cat("The file is type-checked.")
    errors
}

push <- function(xs, el) {
    xs[[length(xs) + 1]] <- el
    return(xs)
}


#' Type check the active file in RStudio
#' @export
type_check_active <- function() {
    x <- rstudioapi::getSourceEditorContext()$contents
    temp_file <- tempfile()
    write(x, file = temp_file)
    type_check(temp_file)
}
