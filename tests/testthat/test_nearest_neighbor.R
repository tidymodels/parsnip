test_that('updating', {
  expect_snapshot(
    nearest_neighbor(neighbors = 5) %>%
      set_engine("kknn", scale = FALSE) %>%
      update(neighbors = tune(), scale = tune())
  )
})

test_that('bad input', {
  expect_error(nearest_neighbor(mode = "reallyunknown"))
  expect_error(nearest_neighbor() %>% set_engine( NULL))
})
