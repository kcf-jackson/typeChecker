f5b <- function(x = ? numeric, y = 10 ? numeric) {
    paste(as.character(x), y)
}
f5b("Hi")

# Check function that returns function
f6 <- function(y) {
    function(x = ? character) {
        x
    }
}
f6(NULL)
f6(NULL)(123)
f6(NULL)("abc")

# Check lexical type
f7 <- function(y = ? character) {
    function(x = ? character) {
        paste(x, y)
    }
}
f7("abc")
f7(NULL)(123)
f7("abc")(123)
f7(NULL)("abc")
