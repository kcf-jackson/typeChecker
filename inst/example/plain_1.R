add <- function(x, y) {
    x + y
}

add(1, 3)    # runs fine
add("a", 3)  # expect type error

x <- "a"
add(x, 3)    # expect type error
