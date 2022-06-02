test_that("printing", {
  expect_snapshot(
    proportional_hazards()
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
    expect_message(
      proportional_hazards() %>% fit_xy()
    ),
    "must use the formula interface"
  )
})
