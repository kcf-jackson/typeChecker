test_that("Test interface", {
    input_file <- system.file("example/test_1.R", package = "typeChecker")

    output <- input_file %>%
        type_check() %>%
        capture.output() %>%
        paste0(collapse = "\n") %>%
        strip_style()

    expected <- "Type mismatch error at 6:1-6:11
In the expression: add('a', 3)
Expected: numeric; Actual: character.
Type mismatch error at 9:1-9:9
In the expression: add(x, 3)
Expected: numeric; Actual: character."

    testthat::expect_equal(output, expected)
})
