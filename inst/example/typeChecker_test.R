# 1. Basic case
f0 <- function(x = ? character) {
    x
}
f0(123)      # fail
f0("abc")    # pass


# 2. Check nested function
nested_f0 <- function(x) {
    f0 <- function(x = ? numeric) {
        x
    }
    f0(x)
}
nested_f0(123)     # pass
nested_f0("abc")   # fail


# 3. Check call within call
f1 <- function() {
    x <- 123
    f0(x)
}
f1()    # fail


# 4. Annotate type of existing functions
print <- function(x = ? character) {
    print(x)
}
print(123)    # fail
print("abc")  # pass


# 5. Check multiple arguments
f2 <- function(x = ? character, y = ? character) {
    paste(x, y)
}
f2(1, 2)       # fail
f2("a", "b")   # pass
f2("a", 2)     # fail
f2(1, "b")     # fail


# 6. Check function overwrite
f2 <- function(z = ? numeric) {
    z
}
f2(123)    # pass
f2("abc")  # fail


# 7. Check empty arguments
f3 <- function() {
    print("hi")
}
f3(123)  # fail

f4 <- function(x = ? character) {
    x
}
f4()  # fail


# 8. Check calling with wrong number of arguments
f5 <- function(x = ? numeric, y = ? character) {
     paste(as.character(x), y)
}
f5("Hi")  # fail


f5b <- function(x = ? numeric, y = 10 ? numeric) {
    paste(as.character(x), y)
}
f5b(10)        # pass
f5b(1, 2)      # pass
f5b("Hi")      # fail
f5b(10, "Hi")  # fail


# 9. Check function that returns function
f6 <- function(y) {
    function(x = ? character) {
        x
    }
}
f6()             # fail
f6(NULL)         # pass
f6(NULL)(123)    # anonymous function does not trigger check

ff6 <- f6(NULL)
ff6(123)         # fail
ff6("abc")       # pass


# 10. Check lexical scope
myPaste <- function(x = ? character, y = ? character) {
    paste(x, y)
}


f7 <- function(y = "abc") {
    y <- 100
    function(x) {
        myPaste(x, y)
    }
}
ff7 <- f7()      # pass
ff7("x")         # fail


f7b <- function(y = "abc") {
    function(x) {
        y <- 100
        myPaste(x, y)
    }
}

ff7 <- f7b()     # pass
ff7("x")         # fail
