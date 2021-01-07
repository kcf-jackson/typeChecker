apply_type <- function(type, expr, envir) {
    # types for assignments
    if (rlang::is_call(expr, c("=", "<-", "<<-"))) {
        return(apply_assignment_type(type, expr, envir))
    }
    # types for function definition
    if (rlang::is_call(expr, "function")) {
        return(apply_fun_def_type(type, expr, envir))
    }
    # types for constant, symbol and other function calls
    # **type application is type checking**
    return(apply_unit_type(type, expr, envir))
}

apply_unit_type <- function(type, expr, envir) {
    res <- eval_type(expr, envir)
    resolved_type <- merge_type(res$value, type,
                                info = list(expr = expr))
    return(store(resolved_type, envir))
}

apply_fun_def_type <- function(type, expr, envir) {
    res <- eval_type(expr, envir)
    f <- res$value
    # new_f :: [type, expr] -> type (See Note 2)
    new_f <- function(input_type, input_expr) {
        fun_infer_type <- f(input_type, input_expr)
        return(merge_type(fun_infer_type, type,
                          info = list(expr = input_expr)))
    }
    return(store(new_f, envir))
}

apply_assignment_type <- function(type, expr, envir) {
    if (!rlang::is_symbol(expr[[2]])) return(store("ANY", envir))

    var <- deparse1(expr[[2]])
    resolved_type <- apply_type(type, expr[[3]], envir)$value
    new_envir <- assign_env(var, resolved_type, envir)
    return(store(resolved_type, new_envir))
}

error_msg <- function(x, y, info) {
    c(sprintf("Type mismatch error at %d:%d-%d:%d",
              attr(info$expr, "line1"), attr(info$expr, "col1"),
              attr(info$expr, "line2"), attr(info$expr, "col2")),
      sprintf("In the expression: %s", attr(info$expr, "text")),
      sprintf("Expected: %s; Actual: %s.", y, x)
    ) %>%
        paste(collapse = "\n") %>%
        cyan()
}

# From the 'crayon' package
cyan <- function(x) paste0("\033[36m", x,"\033[39m")

strip_style <- function(x) {
    ansi_regex <- "(?:(?:\\x{001b}\\[)|\\x{009b})(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])|\\x{001b}[A-M]"
    gsub(ansi_regex, "", x, perl = TRUE)
}

parse_error <- function(x) {
    # Short-hand
    `%o%` <- purrr::compose
    tail1 <- purrr::partial(tail, n = -1)
    num <- as.numeric

    # Helpers
    split_str <- unlist %o% strsplit
    extract_after <- tail1 %o% split_str
    parse_position <- function(x) {
        start <- split_str(x, "-")[1]
        end <- split_str(x, "-")[2]
        list(line1 = num(split_str(start, ":")[1]),
             col1 = num(split_str(start, ":")[2]),
             line2 = num(split_str(end, ":")[1]),
             col2 = num(split_str(end, ":")[2]))
    }

    # Main
    lines <- split_str(strip_style(x), split = "\n")
    pos <- extract_after(lines[1], "Type mismatch error at ")
    expr <- extract_after(lines[2], "In the expression: ")
    error_msg <- paste(lines[1], lines[3], sep = ". ")

    list(expr = expr,
         range = parse_position(pos),
         error_msg = error_msg)
}
