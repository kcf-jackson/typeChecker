test_that("Removing type annotation", {

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
