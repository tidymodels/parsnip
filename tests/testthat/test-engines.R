test_that("NULL engines", {
  # See issue https://github.com/tidymodels/parsnip/issues/1242

  expect_snapshot(
    set_engine(nearest_neighbor(), NULL),
    error = TRUE
  )
})
