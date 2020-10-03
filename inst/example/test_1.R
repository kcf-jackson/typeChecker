add <- function(x = ? numeric, y = ? numeric) {
    x + y
} ? numeric

add(1, 3)    # runs fine
add('a', 3)  # expect type error

x <- 'a'
add(x, 3)    # expect type error
