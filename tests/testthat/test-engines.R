test_that("NULL engines", {
  # See issue https://github.com/tidymodels/parsnip/issues/1242

  expect_snapshot(
    set_engine(nearest_neighbor(), NULL),
    error = TRUE
  )
})

test_that("misspelled engine names give informative error", {
  # See issue https://github.com/tidymodels/parsnip/issues/1110
  skip_if_not_installed("poissonreg")

  expect_snapshot(
    poisson_reg() |>
      set_engine("gml") |>
      set_mode("regression"),
    error = TRUE
  )
})
