test_that("updating", {
  expect_snapshot(
    proportional_hazards() %>%
      update(penalty = tune())
  )
})


test_that("bad input", {
  expect_error(proportional_hazards(mode = ", classification"))
})
