test_that("printing", {
  expect_output(
    print(proportional_hazards()),
    "Proportional Hazards Model Specification \\(censored regression\\)"
  )
})

test_that("updating", {
  basic <- proportional_hazards()
  param_tibb <- tibble::tibble(penalty = 0.05)
  param_list <- as.list(param_tibb)

  expect_snapshot(update(basic, penalty = 0.05))
  expect_snapshot(update(basic, param_tibb))
  expect_snapshot(update(basic, param_list))
})


test_that("bad input", {
  expect_error(proportional_hazards(mode = ", classification"))
})

test_that("wrong fit interface", {
  expect_error(
    proportional_hazards() %>% fit_xy(),
    "must use the formula interface"
  )
})
