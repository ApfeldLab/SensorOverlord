test_that("Error table with two sensors has correct dimensions, pLigand", {
  test_df <- create_error_df_pLigand_multiple(
    c(0.01, 0.02), 2, 10,
    data.frame(
      "Rmin" = c(1, 2),
      "Rmax" = c(5, 6),
      "delta" = c(0.2, 1.2),
      "name" = c("normal", "plusOne"),
      "pKd" = c(7, 8)
    ),
    ligand_name = "NADPH"
  )

  expect_equal(all(dim(test_df) == c(3204, 6)), TRUE)
})
