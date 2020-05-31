#' @export
#' @import vdiffr
#'
test_that("Ranges plot", {
  error_df <- create_error_df_redox_multiple(
    c(0.02, 0.04), -400, -200,
    data.frame(
      Rmin = 0.97, Rmax = 4.12, delta = 0.23, name = "roGFP2", e0 = -299
    )
  )

  # Fraction max plotting
  vdiffr::expect_doppelganger("Ranges, multiple",
                              plot_ranges_redox(create_ranges_multiple(error_df)))
}
)
