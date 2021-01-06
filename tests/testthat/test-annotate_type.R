test_that("Line- and column- numbers are added to parse tree", {
    text <- "a <- f(g(x), g(x))"
    input <- parse(text = text, keep.source = TRUE)
    x <- annotate_expr(input)[[1]]
    testthat::expect_equal(attr(x, "line1"), 1)
    testthat::expect_equal(attr(x, "line2"), 1)
    testthat::expect_equal(attr(x, "col1"), 1)
    testthat::expect_equal(attr(x, "col2"), nchar(text))

    # f(g(x), g(x))
    rhs <- x[[3]]
    testthat::expect_equal(attr(rhs, "line1"), 1)
    testthat::expect_equal(attr(rhs, "line2"), 1)
    testthat::expect_equal(attr(rhs, "col1"), 6)
    testthat::expect_equal(attr(rhs, "col2"), nchar(text))
})
