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
    resolved_type <- merge_type(res$value, type)
    return(store(resolved_type, envir))
}

apply_fun_def_type <- function(type, expr, envir) {
    res <- eval_type(expr, envir)
    f <- res$value
    # new_f :: [type] -> type
    new_f <- function(input_type) {
        fun_infer_type <- f(input_type)
        return(merge_type(fun_infer_type, type))
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

error_msg <- function(x, y) {
    sprintf("Type mismatch. Inferred: %s; Annotated: %s", x, y)
}
