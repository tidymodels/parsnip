hpc <- hpc_data[1:150, c(2:5, 8)]

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

test_that('check_args() works', {
  skip_if_not_installed("kknn")
  
  expect_snapshot(
    error = TRUE,
    {
      spec <- nearest_neighbor(neighbors = -1) %>% 
        set_engine("kknn") %>%
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      spec <- nearest_neighbor(weight_func = 2) %>% 
        set_engine("kknn") %>%
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
})