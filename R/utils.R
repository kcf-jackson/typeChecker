store <- function(x, y) {
    list(value = x, envir = y)
}

new_env <- function() {
    list()
}

assign_env <- function(var, val, envir) {
    envir[[var]] <- val
    return(envir)
}

match_type <- function(x, y) {
    if ("ANY" %in% x || "ANY" %in% y) return(TRUE)
    length(intersect(x, y)) > 0
}

intersect_type <- function(x, y) {
    if ("ANY" %in% x) return(y)
    if ("ANY" %in% y) return(x)
    intersect(x, y)
}

merge_type <- function(x, y, ...) {
    if (!match_type(x, y)) stop(error_msg(x, y, ...))
    return(intersect_type(x, y))
}

add_callback <- function(f, g) {

}
