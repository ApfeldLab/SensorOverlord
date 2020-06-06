test_that("Error table has correct dimensions, pLigand", {
    test_df <- create_error_df_pLigand(c(0.01, 0.02), 2, 10, 1, 5, 0.2, 7,
                                       ligand_name = "NADPH")
    expect_equal(all(dim(test_df) == c(1602, 3)), TRUE)
})

test_that("by argument in error table works, pLigand", {
    test_df <- create_error_df_pLigand(c(0.01, 0.02), 2, 10, 1, 5, 0.2, 7,
                                       ligand_name = "NADPH", by = 0.001)
    expect_equal(all(dim(test_df) == c(16002, 3)), TRUE)
})

test_that("ligand name works in error table for pLigand", {
    test_df <- create_error_df_pLigand(c(0.01, 0.02), 2, 10, 1, 5, 0.2, 7,
                                       ligand_name = "NADPH")
    expect_equal(colnames(test_df)[1], "NADPH")
})


test_that("Error table, pLigand has reasonable first error", {
    test_df <- create_error_df_pLigand(c(0.01, 0.02), 2, 10, 1, 5, 0.2, 7,
                                       ligand_name = "NADPH")
    expect_lt(test_df[1300, 2] - 0.02618501, 0.0001)
})
