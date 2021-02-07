context("Test interface")

test_that("Test interface", {
    input_file <- system.file("example/test_1.R", package = "typeChecker")
    output <- type_check(input_file)
    testthat::expect_equal(output[[1]]$line_number, 6)
    testthat::expect_equal(output[[2]]$line_number, 9)
})

test_that("Test features", {
    input_file <- system.file("example/typeChecker_test.R", package = "typeChecker")
    output <- suppressMessages(type_check(input_file))

    # Example 1
    expect_equal(output[[1]]$line_number, 5)

    # Example 2
    expect_equal(output[[2]]$line_number, 14)

    # Example 3
    expect_equal(output[[3]]$line_number, 23)

    # Example 4
    expect_equal(output[[4]]$line_number, 32)

    # Example 5
    expect_equal(output[[5]]$line_number, 40)
    expect_equal(output[[6]]$line_number, 42)
    expect_equal(output[[7]]$line_number, 43)

    # Example 6
    expect_equal(output[[8]]$line_number, 51)

    # Example 7
    expect_equal(output[[9]]$line_number, 58)
    expect_equal(output[[10]]$line_number, 63)

    # Example 8
    expect_equal(output[[11]]$line_number, 70)
})
