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
