test_that("tabular_chronos() creates a model spec with correct defaults", {
  spec <- tabular_chronos()

  expect_s3_class(spec, "tabular_chronos")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "unknown")
  expect_equal(spec$engine, "brulee")
})

test_that("tabular_chronos() accepts its modes", {
  qr <- tabular_chronos() |>
    set_mode("quantile regression", quantile_levels = c(0.1, 0.5, 0.9))
  expect_equal(qr$mode, "quantile regression")
  expect_equal(qr$quantile_levels, c(0.1, 0.5, 0.9))

  rg <- tabular_chronos() |> set_mode("regression")
  expect_equal(rg$mode, "regression")

  expect_snapshot(error = TRUE, tabular_chronos(mode = "classification"))
})

test_that("update.tabular_chronos() returns a spec", {
  spec <- tabular_chronos()
  updated <- update(spec, parameters = NULL)
  expect_s3_class(updated, "tabular_chronos")
})
