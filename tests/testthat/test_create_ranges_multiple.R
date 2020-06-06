test_that("Ranges are reasonable: test from roGFP2 HansonThesis", {
  error_df <- create_error_df_redox_multiple(
    c(0.02), -400, -200,
    data.frame(
      Rmin = 0.97,
      Rmax = 4.12,
      delta = 0.23,
      name = "roGFP2",
      e0 = -299
    )
  )
  expect_equal(dim(error_df)[1], 20001)
  ranges <- create_ranges_multiple(error_df)

  expect_lt(as.numeric(ranges$Minimum[2]) + 304, 1)
  expect_lt(as.numeric(ranges$Minimum[3]) + 313, 1)
  expect_lt(as.numeric(ranges$Minimum[4]) + 317, 1)
  expect_lt(as.numeric(ranges$Minimum[5]) + 321, 1)

  expect_lt(as.numeric(ranges$Maximum[2]) + 274, 1)
  expect_lt(as.numeric(ranges$Maximum[3]) + 266, 1)
  expect_lt(as.numeric(ranges$Maximum[4]) + 261, 1)
  expect_lt(as.numeric(ranges$Maximum[5]) + 258, 1)

})

test_that("Ranges are reasonable for a pH sensor", {
  error_df <- create_error_df_pH_multiple(
    c(0.01, 0.02), 1, 12,
    data.frame(
      "Rmin" = c(1, 2),
      "Rmax" = c(5, 6),
      "delta" = c(0.2, 1.2),
      "name" = c("normal", "plusOne"),
      "pKa" = c(7, 8)
    )
  )
  expect_equal(dim(error_df)[1], 4404)
  ranges <- create_ranges_multiple(error_df, parameter = 'pH')

  expect_lt(as.numeric(ranges$Minimum[1]) - 4.57, 0.001)
  expect_lt(as.numeric(ranges$Minimum[2]) - 4.45, 0.001)
  expect_lt(as.numeric(ranges$Minimum[3]) - 4.42, 0.001)
  expect_lt(as.numeric(ranges$Minimum[4]) - 4.41, 0.001)

  expect_lt(as.numeric(ranges$Maximum[1]) - 8.72, 0.001)
  expect_lt(as.numeric(ranges$Maximum[2]) - 8.85, 0.001)
  expect_lt(as.numeric(ranges$Maximum[3]) - 8.88, 0.001)
  expect_lt(as.numeric(ranges$Maximum[4]) - 8.89, 0.001)

})

test_that("Ranges are reasonable for a pLigand sensor", {
  error_df <- create_error_df_pLigand_multiple(
    c(0.01, 0.02), 1, 12,
    data.frame(
      "Rmin" = c(1, 2),
      "Rmax" = c(5, 6),
      "delta" = c(0.2, 1.2),
      "name" = c("normal", "plusOne"),
      "pKd" = c(7, 8)
    ),
    ligand_name = "NADPH"
  )
  expect_equal(dim(error_df)[1], 4404)
  ranges <- create_ranges_multiple(error_df, parameter = 'NADPH')

  expect_lt(as.numeric(ranges$Minimum[1]) - 4.57, 0.001)
  expect_lt(as.numeric(ranges$Minimum[2]) - 4.45, 0.001)
  expect_lt(as.numeric(ranges$Minimum[3]) - 4.42, 0.001)
  expect_lt(as.numeric(ranges$Minimum[4]) - 4.41, 0.001)

  expect_lt(as.numeric(ranges$Maximum[1]) - 8.72, 0.001)
  expect_lt(as.numeric(ranges$Maximum[2]) - 8.85, 0.001)
  expect_lt(as.numeric(ranges$Maximum[3]) - 8.88, 0.001)
  expect_lt(as.numeric(ranges$Maximum[4]) - 8.89, 0.001)
})
