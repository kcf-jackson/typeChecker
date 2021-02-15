#' Find the type of an expression
#'
#' @param expr An R expression; an object returned from \link[rlang]{parse_expr}.
#' @param envir A named list; the environment in which the expression is evaluated.
#'
#' @export
eval_type <- function(expr, envir = new_env()) {
    # types for constant
    if (rlang::is_syntactic_literal(expr)) {
        return(eval_constant_type(expr, envir))
    }
    # types for symbol
    if (rlang::is_symbol(expr)) {
        return(eval_symbol_type(expr, envir))
    }
    # types for assignments
    if (rlang::is_call(expr, c("=", "<-", "<<-"))) {
        return(eval_assignment_type(expr, envir))
    }
    # types for closure
    if (rlang::is_call(expr, "{")) {
        return(eval_closure_type(expr, envir))
    }
    # types for function definition
    if (rlang::is_call(expr, "function")) {
        return(eval_fun_def_type(expr, envir))
    }
    # types for annotations
    if (rlang::is_call(expr, "?")) {
        return(eval_annotation_type(expr, envir))
    }
    # types for all the remaining function calls
    return(eval_fun_call_type(expr, envir))
}

eval_constant_type <- function(expr, envir) {
    if (is.numeric(expr))   return(store("numeric", envir))
    if (is.character(expr)) return(store("character", envir))
    if (is.logical(expr))   return(store("logical", envir))
    if (is.null(expr))      return(store("NULL", envir))
}

eval_symbol_type <- function(expr, envir) {
    arg <- deparse1(expr)
    val <- envir[[arg]]
    if (!is.null(val)) return(store(val, envir))
    return(store("ANY", envir))
}

eval_assignment_type <- function(expr, envir) {
    # LHS must be a variable, not something like attribute assignment,
    # e.g. names(x) <- 'abc' is assigned no type or any type.
    if (!rlang::is_symbol(expr[[2]])) return(store("ANY", envir))

    # Evaluate the type on the RHS and assign to the LHS.
    var <- deparse1(expr[[2]])
    type <- eval_type(expr[[3]], envir)$value
    return(store(type, assign_env(var, type, envir)))
}

eval_annotation_type <- function(expr, envir) {
    # Length-two call occurs at pairlist
    if (length(expr) == 2) return(store(deparse1(expr[[2]]), envir))
    # All other cases should have length three
    type <- deparse1(expr[[3]])  # TODO: handling union type here
                                 # p("x ? numeric || character || logical")[[3]]
    return(apply_type(type = type, expr = expr[[2]], envir))
}

eval_closure_type <- function(expr, envir) {
    if (length(expr) == 1) return(store(NULL, envir))
    for (i in 2:length(expr)) {
        res <- eval_type(expr[[i]], envir)
        val <- res$value
        envir <- res$envir
    }
    return(store(val, envir))
}

# It is helpful to think that the type of a function is a function, rather than
# the input-output declaration, as we are in the context of dynamic typing.
eval_fun_def_type <- function(expr, envir) {
    pairlist_args <- expr[[2]]
    pairlist_type <- purrr::map(expr[[2]], ~eval_type(.x, envir = envir)$value)  # assume default value implies type constraints
    fun_body <- expr[[3]]
    # f :: [type, expr] -> type
    # Note 2: the function is extended by an argument 'input_expr' to pinpoint
    # the expression when the type-check fails.
    f <- function(input_type, input_expr) {
        check_matching_numbers_of_arguments(
            input_type, pairlist_args,
            info = list(expr = input_expr)
        )
        input_type <- add_full_names(input_type, names(pairlist_type))
        restricted_input <- purrr::map2(
            input_type, pairlist_type, merge_type,
            info = list(expr = input_expr)
        )
        eval_type(fun_body, append(envir, restricted_input))$value
    }
    return(store(f, envir))
}

eval_fun_call_type <- function(expr, envir) {
    fun_name <- deparse1(expr[[1]])
    fun <- envir[[fun_name]]
    # Type check the arguments
    input_type <- purrr::map(expr[-1], ~eval_type(.x, envir = envir)$value)
    # The type signature of a function is declared
    if (!is.null(fun)) {
        return(store(fun(input_type, expr), envir))  # See Note 2
    }
    # The type signature of a function is not declared
    return(store("ANY", envir))
}

add_full_names <- function(args, args_names) {
    nargs <- names(args)
    if (is.null(nargs)) {
        replace_seq <- seq_along(args)
        names(args) <- args_names[replace_seq]
        return(args)
    } else {
        replace_seq <- seq_along(names(args)[nargs == ""])
        names(args)[nargs == ""] <- setdiff(args_names, nargs)[replace_seq]
        return(args)
    }
}

check_matching_numbers_of_arguments <- function(input_type, pairlist_args, info) {
    min_length <- num_free_args(pairlist_args)
    max_length <- length(pairlist_args)
    input_length <- length(input_type)

    too_few <- input_length < min_length
    too_many <- input_length > max_length
    if (!too_few && !too_many) return(TRUE)

    # Provide more information when mismatch occurs
    detail <- ifelse(
        too_few,
        sprintf("Expected: at least %s; Actual: %s.", min_length, input_length),
        sprintf("Expected: at most %s; Actual: %s.", max_length, input_length)
    )

    error_message <- c(
        sprintf("Mismatch of numbers of arguments at %d:%d-%d:%d",
           attr(info$expr, "line1"), attr(info$expr, "col1"),
           attr(info$expr, "line2"), attr(info$expr, "col2")),
        sprintf("In the expression: %s", attr(info$expr, "text")),
        detail
    ) %>%
        paste(collapse = "\n") %>%
        cyan()
    stop(error_message)
}

# Number of arguments without default values
num_free_args <- function(pairlist_args) {
    has_default_value <- function(arg) {
        # case with type annotation
        if (rlang::is_call(arg, "?")) {
            return(length(arg) >= 3)
        }
        # regular case
        deparse1(arg) != ""
    }

    pairlist_args %>%
        purrr::map_lgl(~!has_default_value(.x)) %>%
        sum()
}
