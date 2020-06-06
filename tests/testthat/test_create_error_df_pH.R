test_that("Error table has correct dimensions, pH", {
    test_df <- create_error_df_pH(c(0.01, 0.02), 2, 10, 1, 5, 0.2, 7)
    expect_equal(all(dim(test_df) == c(1602, 3)), TRUE)
})

test_that("by argument in error table works, pH", {
    test_df <- create_error_df_pH(c(0.01, 0.02), 2, 10, 1, 5, 0.2, 7, by = 0.001)
    expect_equal(all(dim(test_df) == c(16002, 3)), TRUE)
})


test_that("Error table, pH has reasonable first error", {
    test_df <- create_error_df_pH(c(0.01, 0.02), 2, 10, 1, 5, 0.2, 7)
    expect_lt(test_df[1300, 2] - 0.02618501, 0.0001)
})
