test_that('updating', {
  expect_snapshot(
    nearest_neighbor(neighbors = 5) |>
      set_engine("kknn", scale = FALSE) |>
      update(neighbors = tune(), scale = tune())
  )
})

test_that('bad input', {
  expect_snapshot(error = TRUE, nearest_neighbor(mode = "reallyunknown"))
})

test_that('check_args() works', {
  skip_if_not_installed("kknn")
  skip_if_not_installed("modeldata")

  hpc <- hpc_data[1:150, c(2:5, 8)]

  expect_snapshot(
    error = TRUE,
    {
      spec <- nearest_neighbor(neighbors = -1) |>
        set_engine("kknn") |>
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      spec <- nearest_neighbor(weight_func = 2) |>
        set_engine("kknn") |>
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
})
