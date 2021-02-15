test_that("Removing type annotation", {
    input_file <- system.file("example/test_1.R", package = "typeChecker")
    ref_file <- system.file("example/plain_1.R", package = "typeChecker")

    output <- input_file %>%
        remove_types_from_file() %>%
        capture.output()

    expected <- ref_file %>%
        readLines()

    testthat::expect_identical(output, expected)
})

test_that("Handling trailing comments", {
    testthat::expect_equal(
        safe_paste("abc", list(start_pos = 5, text = "# 123")),
        "abc # 123"
    )

    testthat::expect_error(
        safe_paste("abcde", list(start_pos = 5, text = "# 123"))
    )

    testthat::expect_equal(
        safe_paste("abc", NULL),
        "abc"
    )
})
