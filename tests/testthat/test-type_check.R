p <- rlang::parse_expr

testthat::test_that("Test type evaluations - constants", {
    res <- eval_type(p("3"))
    testthat::expect_equal(res$value, "numeric")

    res <- eval_type(p("'Hello World!'"))
    testthat::expect_equal(res$value, "character")

    res <- eval_type(p("TRUE"))
    testthat::expect_equal(res$value, "logical")

    res <- eval_type(p("NULL"))
    testthat::expect_equal(res$value, "NULL")
})

testthat::test_that("Test type evaluations - symbols", {
    res <- eval_type(p("x"))
    testthat::expect_equal(res$value, "ANY")

    res <- eval_type(p("x"), list(x = "numeric"))
    testthat::expect_equal(res$value, "numeric")
})

testthat::test_that("Test type evaluations - assignment", {
    res <- eval_type(p("x <- 3"))
    testthat::expect_equal(res$value, "numeric")
    testthat::expect_equal(res$envir$x, "numeric")

    res <- eval_type(p("x <- 'Hello World!'"))
    testthat::expect_equal(res$value, "character")
    testthat::expect_equal(res$envir$x, "character")

    res <- eval_type(p("x <- TRUE"))
    testthat::expect_equal(res$value, "logical")
    testthat::expect_equal(res$envir$x, "logical")

    res <- eval_type(p("x <- NULL"))
    testthat::expect_equal(res$value, "NULL")
    testthat::expect_equal(res$envir$x, "NULL")

    res <- eval_type(p("x <- y"))
    testthat::expect_equal(res$value, "ANY")
    testthat::expect_equal(res$envir$x, "ANY")

    res <- eval_type(p("x <- y"), list(y = "numeric"))
    testthat::expect_equal(res$value, "numeric")
    testthat::expect_equal(res$envir$x, "numeric")

    res <- eval_type(p("x <- y"), list(y = c("numeric", "character")))
    testthat::expect_equal(res$value, c("numeric", "character"))
    testthat::expect_equal(res$envir$x, c("numeric", "character"))
    testthat::expect_equal(res$envir$y, c("numeric", "character"))
})

testthat::test_that("Test type evaluations - closure", {
    res <- eval_type(p("{}"))
    testthat::expect_equal(res$value, NULL)
    testthat::expect_equal(length(res$envir), 0)

    res <- eval_type(p("{x <- 3}"))
    testthat::expect_equal(res$envir$x, "numeric")
    testthat::expect_equal(res$value, "numeric")
    testthat::expect_equal(length(res$envir), 1)

    res <- eval_type(p("{x <- 3; y <- 'a'}"))
    testthat::expect_equal(res$envir$x, "numeric")
    testthat::expect_equal(res$envir$y, "character")
    testthat::expect_equal(res$value, "character")
    testthat::expect_equal(length(res$envir), 2)

    res <- eval_type(p("{x <- 3; y <- x}"))
    testthat::expect_equal(res$envir$x, "numeric")
    testthat::expect_equal(res$envir$y, "numeric")
    testthat::expect_equal(res$value, "numeric")
    testthat::expect_equal(length(res$envir), 2)

    res <- eval_type(p("{x <- z; y <- 3}"))
    testthat::expect_equal(res$envir$x, "ANY")
    testthat::expect_equal(res$envir$y, "numeric")
    testthat::expect_equal(res$value, "numeric")
    testthat::expect_equal(length(res$envir), 2)

    res <- eval_type(p("{y <- 3; x <- z}"))
    testthat::expect_equal(res$envir$x, "ANY")
    testthat::expect_equal(res$envir$y, "numeric")
    testthat::expect_equal(res$value, "ANY")
    testthat::expect_equal(length(res$envir), 2)
})


testthat::test_that("Test type evaluations - function definition", {
    res <- eval_type(p("function(x) x"))
    testthat::expect_equal(res$value("numeric"), "numeric")
    testthat::expect_equal(res$value("character"), "character")

    res <- eval_type(p("function(x) x + 1"))
    testthat::expect_equal(res$value("numeric"), "ANY")
    testthat::expect_equal(res$value("character"), "ANY")

    # res <- eval_type(p("function(x) x + 1"))
    # testthat::expect_equal(res$value(3), "ANY")
    # testthat::expect_equal(res$value('a'), "ANY")
    #
    # res <- eval_type(p("function(x) { x + 1 }"))
    # res <- eval_type(p("function(x) { y <- x + 1; 2 * y }"))
})


testthat::test_that("Test type evaluations - function evaluation", {
    res <- eval_type(p("identity <- function(x) x"))
    testthat::expect_equal(length(res$envir), 1)
    res2 <- eval_type(p("identity(3)"), res$envir)
    testthat::expect_equal(res2$value, "numeric")
    res2 <- eval_type(p("identity('a')"), res$envir)
    testthat::expect_equal(res2$value, 'character')
    res2 <- eval_type(p("identity(y)"), res$envir)
    testthat::expect_equal(res2$value, "ANY")
    res2 <- eval_type(p("identity(y)"), append(res$envir, list(y = "logical")))
    testthat::expect_equal(res2$value, "logical")

    res2 <- eval_type(p("identity(y)"),
                      append(res$envir, list(y = c("logical", "character"))))
    testthat::expect_equal(res2$value, c("logical", "character"))

    res <- eval_type(p("function(x) x + 1"))
    testthat::expect_equal(res$value("numeric"), "ANY")
    res <- eval_type(p("function(x, y) x + y"))
    testthat::expect_equal(res$value(c("numeric", "numeric")), "ANY")

    res <- eval_type(p("identity <- function(x) { y <- x + 1; x }"))
    res2 <- eval_type(p("identity(3)"), res$envir)
    testthat::expect_equal(res2$value, "numeric")
    res2 <- eval_type(p("identity('a')"), res$envir)
    testthat::expect_equal(res2$value, 'character')
})


testthat::test_that("Test type evaluations - annotation", {
    # Test annotation for constants
    res <- eval_type(p("x <- 3 ? numeric"))
    testthat::expect_equal(res$value, "numeric")
    testthat::expect_equal(res$envir$x, "numeric")
    testthat::expect_error(eval_type(p("x <- 3 ? character")))
    testthat::expect_equal(length(res$envir), 1)

    # Test annotation for variables
    res <- eval_type(p("x <- y ? numeric"))
    testthat::expect_equal(res$value, "numeric")
    testthat::expect_equal(res$envir$x, "numeric")
    testthat::expect_equal(length(res$envir), 1)

    # Test annotation for multiple expressions
    res <- eval_type(p("{
                       x <- y ? numeric;
                       z <- x;
                       }"))
    testthat::expect_equal(res$value, "numeric")
    testthat::expect_equal(res$envir$x, "numeric")
    testthat::expect_equal(res$envir$z, "numeric")
    testthat::expect_equal(length(res$envir), 2)

    # Test annotation for function definition
    res <- eval_type(p("function(x, y) { x } ? numeric"))
    testthat::expect_equal(res$value(c("numeric", "numeric")), "numeric")
    testthat::expect_error(res$value(c("character", "character")))

    res <- eval_type(p("function(x = ? numeric, y) { x + y }"))
    testthat::expect_equal(res$value(c("numeric", "numeric")), "ANY")
    testthat::expect_error(res$value(c("character", "character")))

    res <- eval_type(p("function(x, y) { x + y } ? numeric"))
    testthat::expect_equal(res$value(c("numeric", "numeric")), "numeric")
    testthat::expect_equal(res$value(c("character", "character")), "numeric")

    res <- eval_type(p("add <- function(x, y) { x + y } ? numeric"))
    testthat::expect_equal(res$value(c("numeric", "numeric")), "numeric")
    testthat::expect_equal(res$envir$add(c("numeric", "numeric")), "numeric")
    testthat::expect_equal(res$value(c("character", "character")), "numeric")

    # Test annotation for function calls
    res <- eval_type(p("{
                       add <- function(x, y) { x + y } ? numeric;
                       add(1, 2);
                       }"))
    testthat::expect_equal(res$value, "numeric")
    res <- eval_type(p("{
                       mypaste <- function(x, y) { paste0(x, y) } ? character;
                       mypaste('a', 'b');
                       }"))
    testthat::expect_equal(res$value, "character")

    res <- eval_type(p("{
                       mypaste <- function(x, y) { paste0(x, y) } ? character;
                       concat <- function(x, y) { mypaste(x, y) } ? numeric;
                       }"))
    res2 <- eval_type(p("mypaste('a', 'b')"), res$envir)
    testthat::expect_equal(res2$value, "character")
    testthat::expect_error(eval_type(p("concat('a', 'b')"), res$envir))

    # Test annotation for function arguments
    res <- eval_type(p("add <- function(x = ? numeric, y) { x + y }"))
    res2 <- eval_type(p("add(1, 3)"), res$envir)
    testthat::expect_equal(res2$value, "ANY")
    testthat::expect_error(eval_type(p("add('a', 3)"), res$envir))

    res <- eval_type(p("add <- function(x = ? numeric, y = ? numeric) { x + y } ? numeric"))
    res2 <- eval_type(p("add(1, 3)"), res$envir)
    testthat::expect_equal(res2$value, "numeric")
    testthat::expect_error(eval_type(p("add('a', 3)"), res$envir))

    # testthat::expect_equal(res$value, "numeric")
    # res2 <- eval_type(p("add(1,2)"), res$envir)
    # testthat::expect_equal(res$value, "numeric")
    # testthat::expect_equal(res$envir$x, "numeric")
    # testthat::expect_equal(res$envir$z, "numeric")
    # testthat::expect_equal(length(res$envir), 2)
})
