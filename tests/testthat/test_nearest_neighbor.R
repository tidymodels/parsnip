test_that('primary arguments', {
  basic <- nearest_neighbor(mode = "regression")
  basic_kknn <- translate(basic %>% set_engine("kknn"))

  expect_equal(
    object = basic_kknn$method$fit$args,
    expected = list(
      formula = expr(missing_arg()),
      data = expr(missing_arg()),
      ks = expr(min_rows(5, data, 5))
    )
  )

  neighbors <- nearest_neighbor(mode = "classification", neighbors = 2)
  neighbors_kknn <- translate(neighbors %>% set_engine("kknn"))

  expect_equal(
    object = neighbors_kknn$method$fit$args,
    expected = list(
      formula = expr(missing_arg()),
      data = expr(missing_arg()),
      ks = expr(min_rows(2, data, 5))
    )
  )

  weight_func <- nearest_neighbor(mode = "classification", weight_func = "triangular")
  weight_func_kknn <- translate(weight_func %>% set_engine("kknn"))

  expect_equal(
    object = weight_func_kknn$method$fit$args,
    expected = list(
      formula = expr(missing_arg()),
      data = expr(missing_arg()),
      kernel = new_empty_quosure("triangular"),
      ks = expr(min_rows(5, data, 5))
    )
  )

  dist_power <- nearest_neighbor(mode = "classification", dist_power = 2)
  dist_power_kknn <- translate(dist_power %>% set_engine("kknn"))

  expect_equal(
    object = dist_power_kknn$method$fit$args,
    expected = list(
      formula = expr(missing_arg()),
      data = expr(missing_arg()),
      distance = new_empty_quosure(2),
      ks = expr(min_rows(5, data, 5))
    )
  )

})

test_that('engine arguments', {

  kknn_scale <- nearest_neighbor(mode = "classification") %>% set_engine("kknn", scale = FALSE)

  expect_equal(
    object = translate(kknn_scale, "kknn")$method$fit$args,
    expected = list(
      formula = expr(missing_arg()),
      data = expr(missing_arg()),
      scale = new_empty_quosure(FALSE),
      ks = expr(min_rows(5, data, 5))
    )
  )

})


test_that('updating', {

  expr1     <- nearest_neighbor()  %>% set_engine("kknn", scale = FALSE)
  expr1_exp <- nearest_neighbor(neighbors = 5) %>% set_engine("kknn", scale = FALSE)

  expr2     <- nearest_neighbor(neighbors = tune()) %>% set_engine("kknn", scale = tune())
  expr2_exp <- nearest_neighbor(neighbors = tune(), weight_func = "triangular") %>% set_engine("kknn", scale = FALSE)

  expr3     <- nearest_neighbor(neighbors = 2, weight_func = tuns()) %>% set_engine("kknn", scale = tune())
  expr3_exp <- nearest_neighbor(neighbors = 3) %>% set_engine("kknn", scale = FALSE)

  expect_equal(update(expr1, neighbors = 5, scale = FALSE), expr1_exp)
  expect_equal(update(expr2, weight_func = "triangular", scale = FALSE), expr2_exp)
  expect_equal(update(expr3, neighbors = 3, fresh = TRUE, scale = FALSE), expr3_exp)

  param_tibb <- tibble::tibble(neighbors = 7, dist_power = 1)
  param_list <- as.list(param_tibb)

  expr1_updated <- update(expr1, param_tibb)
  expect_equal(expr1_updated$args$neighbors, 7)
  expect_equal(expr1_updated$args$dist_power, 1)
  expect_equal(expr1_updated$eng_args$scale, rlang::quo(FALSE))

  expr1_updated_lst <- update(expr1, param_list)
  expect_equal(expr1_updated_lst$args$neighbors, 7)
  expect_equal(expr1_updated_lst$args$dist_power, 1)
  expect_equal(expr1_updated_lst$eng_args$scale, rlang::quo(FALSE))

  expr1_updated_mod <- update(expr1, param_list, neighbors = 3)
  expect_equal(expr1_updated_mod$args$neighbors, 7)
  expect_equal(expr1_updated_mod$args$dist_power, 1)
  expect_equal(expr1_updated_mod$eng_args$scale, rlang::quo(FALSE))

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
