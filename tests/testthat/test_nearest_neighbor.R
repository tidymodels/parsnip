test_that('updating', {
  expr1 <- nearest_neighbor()  %>% set_engine("kknn", scale = FALSE)
  expr2 <- nearest_neighbor(neighbors = tune()) %>% set_engine("kknn", scale = tune())
  expr3 <- nearest_neighbor(neighbors = 2, weight_func = tuns()) %>% set_engine("kknn", scale = tune())

  param_tibb <- tibble::tibble(neighbors = 7, dist_power = 1)
  param_list <- as.list(param_tibb)

  expect_snapshot(expr1 %>% update(neighbors = 5, scale = FALSE))
  expect_snapshot(expr1 %>% update(param_tibb))
  expect_snapshot(expr1 %>% update(param_list))
  expect_snapshot(expr1 %>% update(param_list, neighbors = 3))
  expect_snapshot(expr2 %>% update(weight_func = "triangular", scale = FALSE))
  expect_snapshot(expr3 %>% update(neighbors = 3, fresh = TRUE, scale = FALSE))

  param_tibb$scale <- TRUE
  expect_error(
    update(expr1, param_tibb),
    "At least one argument is not a main argument"
  )
})

test_that('bad input', {
  expect_error(nearest_neighbor(mode = "reallyunknown"))
  expect_error(nearest_neighbor() %>% set_engine( NULL))
})
