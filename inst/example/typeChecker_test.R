# 1. Basic case
f0 <- function(x = ? character) {
    x
}
f0(123)
f0("abc")


# 2. Check nested function
nested_f0 <- function(x) {
    f0 <- function(x = ? numeric) {
        x
    }
    f0(x)
}
nested_f0(123)
nested_f0("abc")


# 3. Check call within call
f1 <- function() {
    x <- 123
    f0(x)
}
f1()


# 4. Annotate type of existing functions
print <- function(x = ? character) {
    print(x)
}
print(123)
print("abc")


# 5. Check multiple arguments
f2 <- function(x = ? character, y = ? character) {
    paste(x, y)
}
f2(1, 2)
f2("a", "b")
f2("a", 2)
f2(1, "b")


# 6. Check function overwrite
f2 <- function(z = ? numeric) {
    z
}
f2(123)
f2("abc")


# 7. Check empty arguments
f3 <- function() {
    print("hi")
}
f3(123)

f4 <- function(x = ? character) {
    x
}
f4()


# 8. Check calling with wrong number of arguments
f5 <- function(x = ? numeric, y = ? character) {
     paste(as.character(x), y)
}
f5("Hi")
