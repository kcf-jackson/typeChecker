dog <- function(name = ? character) {
    list(name = name) ? dog
}
introduce <- function(x = ? dog) {
    sprintf("Woof! My name is %s!", x$name)
}

x <- dog("Napawleon")
introduce(x)             # correct usage
introduce("Pawgustus")   # type error
# In the expression:
# introduce("Pawgustus")
# The following type error is found:
# Type mismatch. Inferred: character; Annotated: dog
