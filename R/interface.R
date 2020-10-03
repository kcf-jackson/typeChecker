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
    es <- rlang::parse_exprs(file(path))
    envir <- list()
    safe_eval_type <- purrr::safely(eval_type)
    for (expr in es) {
        res <- safe_eval_type(expr, envir)
        if (!is.null(res$error)) {
            cat("In expression:\n")
            print(expr)
            cat("The following type error is found:\n")
            cat(res$error$message)
            break
        } else {
            envir <- res$result$envir
        }
    }
    invisible(NULL)
}

push <- function(xs, el) {
    xs[[length(xs) + 1]] <- el
    return(xs)
}
