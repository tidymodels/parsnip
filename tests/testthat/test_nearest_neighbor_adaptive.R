library(tidydann)

test_that("updating", {
  expect_snapshot(
    nearest_neighbor_adaptive(neighbors = 5) %>%
      set_engine("dann", neighborhood_size = 10) %>%
      update(neighbors = tune(), neighborhood_size = tune())
  )
})

test_that("bad input", {
  expect_error(nearest_neighbor_adaptive(mode = "reallyunknown"))
  expect_error(nearest_neighbor_adaptive() %>% set_engine(NULL))
})
