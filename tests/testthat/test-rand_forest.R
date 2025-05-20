
test_that('updating', {
  expect_snapshot(
    rand_forest(mode = "regression", mtry = 2) |>
      set_engine("randomForest", sampsize = 10) |>
      update(mtry = tune(), sampsize = tune())
  )
})

test_that('bad input', {
  expect_snapshot(res <-
                    translate(rand_forest(mode = "classification") |>
                                set_engine(NULL)),
                  error = TRUE)
  expect_snapshot(error = TRUE, rand_forest(mode = "time series"))
  expect_snapshot(error = TRUE, translate(rand_forest(mode = "classification") |> set_engine("wat?")))
})

test_that("check_args() works", {
  # Here for completeness, no checking is done
  expect_true(TRUE)
})
