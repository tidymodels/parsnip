test_that("printing", {
  expect_output(
    print(proportional_hazards()),
    "Proportional Hazards Model Specification \\(censored regression\\)"
  )
})

test_that("updating", {
  expect_snapshot(
    proportional_hazards() %>%
      update(penalty = tune())
  )
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
