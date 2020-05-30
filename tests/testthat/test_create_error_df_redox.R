test_that("Error table has correct dimensions", {
    test_df <- create_error_df_redox(c(0.01, 0.02), -300, -200, 1, 5, 0.2, -275)

    expect_equal(all(dim(test_df) == c(20002, 3)), TRUE)
})
