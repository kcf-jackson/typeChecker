p <- rlang::parse_expr

testthat::test_that("Test apply_type", {
    res <- purrr::map(p("function(x = 3){}")[[2]], eval_type)$x
    testthat::expect_equal(res$value, "numeric")

    res <- purrr::map(p("function(x = ? character){}")[[2]], eval_type)$x
    testthat::expect_equal(res$value, "character")

    res <- purrr::map(p("function(x = 'a' ? character){}")[[2]], eval_type)$x
    testthat::expect_equal(res$value, "character")

    testthat::expect_error(
        purrr::map(p("function(x = 3 ? character){}")[[2]], eval_type)$x
    )

    res <- purrr::map(p("function(x = y ? logical){}")[[2]], eval_type)$x
    testthat::expect_equal(res$value, "logical")

    # res <- purrr::map(p("function(x = y ? logical){}")[[2]], eval_type,
    #                   env = list(y = c("logical", "numeric")))$x
    # testthat::expect_equal(res$value, "logical")
})
