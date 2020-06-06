#' @export
#' @import vdiffr
#'
test_that("Ranges plot", {
  error_df <- create_error_df_pH_multiple(
    c(0.01, 0.02), 2, 10,
    data.frame(
      "Rmin" = c(1, 2),
      "Rmax" = c(5, 6),
      "delta" = c(0.2, 1.2),
      "name" = c("normal", "plusOne"),
      "pKa" = c(7, 8)
    )
  )
  ranges_df <- create_ranges_multiple(error_df, parameter = 'pH',
                                      thresholds = c(0.01, 0.05, 0.10, 0.15, 0.20))
  plot <- plot_ranges_pH(ranges_df, by = 1)

  # Fraction max plotting
  vdiffr::expect_doppelganger("Ranges, multiple, pH",
                              plot)
}
)
