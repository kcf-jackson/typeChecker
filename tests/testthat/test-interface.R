test_that("Test interface", {
    input_file <- system.file("example/test_1.R", package = "typeChecker")

    output <- input_file %>%
        type_check() %>%
        capture.output() %>%
        paste0(collapse = "\n")

    expected <- "In the expression:
add('a', 3)
The following type error is found:
Type mismatch. Inferred: character; Annotated: numeric"

    testthat::expect_equal(output, expected)
})
