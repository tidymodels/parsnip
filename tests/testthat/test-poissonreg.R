test_that("misspelled engine names give informative error for extension packages", {
  # See issue https://github.com/tidymodels/parsnip/issues/1110
  skip_if_not_installed("poissonreg")

  expect_snapshot(
    poisson_reg() |>
      set_engine("gml") |>
      set_mode("regression"),
    error = TRUE
  )
})
