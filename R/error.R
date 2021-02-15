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

# Top-level parser of error messages
parse_error <- function(x) {
    lines <- split_str(strip_style(x), split = "\n")
    if (grepl("Mismatch of numbers of arguments", lines[1])) {
        return(parse_arg_num_error(x))
    }
    if (grepl("Type mismatch error at ", lines[1])) {
        return(parse_type_error(x))
    }
    stop("Uncaught error:", x)
}

# Worker parser for type error
parse_type_error <- function(x) {
    lines <- split_str(strip_style(x), split = "\n")
    pos <- extract_after(lines[1], "Type mismatch error at ")
    expr <- extract_after(lines[2], "In the expression: ")
    error_msg <- paste(lines[1], lines[3], sep = ". ")

    list(expr = expr,
         range = parse_position(pos),
         error_msg = error_msg)
}

# Worker parser for number-of-argument-mismatch error
parse_arg_num_error <- function(x) {
    lines <- split_str(strip_style(x), split = "\n")
    pos <- extract_after(lines[1], "Mismatch of numbers of arguments at ")
    expr <- extract_after(lines[2], "In the expression: ")
    error_msg <- paste(lines[1], lines[3], sep = ". ")

    list(expr = expr,
         range = parse_position(pos),
         error_msg = error_msg)
}
