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
type_check <- function(path) {
    if (grepl("\n", path)) {
        es <- parse(text = path, keep.source = TRUE)
    } else {
        es <- parse(path, keep.source = TRUE)
    }
    aes <- annotate_expr(es)

    safe_eval_type <- purrr::safely(eval_type)
    envir <- list()
    errors <- list()
    for (expr in aes) {
        res <- safe_eval_type(expr, envir)
        if (!is.null(res$error)) {
            cat(res$error$message, "\n")
            info <- parse_error(res$error$message)
            errors <- c(errors, list(list(
                filename = basename(path),
                pathname = path,
                line_number = info$range$line1,
                column_number = info$range$col1,
                type = "type",
                message = info$error_msg,
                line = info$expr,
                ranges = list(c(info$range$col1, info$range$col2)),
                linter = "typeChecker"
            )))
        } else {
            envir <- res$result$envir
        }
    }
    if (length(errors) > 0) {
        return(invisible(errors))
    } else {
        cat("The file is type-checked.")
        return(invisible(NULL))
    }
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
