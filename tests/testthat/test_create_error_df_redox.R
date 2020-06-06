test_that("Error table has correct dimensions", {
    test_df <- create_error_df_redox(c(0.01, 0.02), -300, -200, 1, 5, 0.2, -275)
    expect_equal(all(dim(test_df) == c(20002, 3)), TRUE)
})

test_that("Error table, redox has reasonable first error", {
    test_df <- create_error_df_redox(c(0.01, 0.02), -300, -200, 1, 5, 0.2, -275)
    expect_lt(test_df$Error[1] - 1.400896, 0.0001)
})
