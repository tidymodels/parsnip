hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('engine arguments', {
  rpart_print <- decision_tree(mode = "regression")
  expect_equal(
    translate(
      rpart_print %>%
        set_engine("rpart", model = TRUE))$method$fit$args,
    list(
      formula = expr(missing_arg()),
      data = expr(missing_arg()),
      weights = expr(missing_arg()),
      model = quo(TRUE)
    )
  )

  C5.0_rules <- decision_tree(mode = "classification")
  expect_equal(
    translate(
      C5.0_rules %>% set_engine("C5.0", rules = TRUE))$method$fit$args,
    list(
      x = expr(missing_arg()),
      y = expr(missing_arg()),
      weights = expr(missing_arg()),
      rules = quo(TRUE),
      trials = 1
    )
  )

})


test_that('updating', {
  expr1     <- decision_tree() %>% set_engine("rpart", model = FALSE)
  expr1_exp <- decision_tree(cost_complexity = .1) %>% set_engine("rpart", model = FALSE)

  expr2     <- decision_tree(cost_complexity = tune()) %>% set_engine("rpart", model = tune())
  expr2_exp <- decision_tree(cost_complexity = tune()) %>% set_engine("rpart", model = FALSE)

  expr3     <- decision_tree(cost_complexity = 1, min_n = tune())
  expr3_exp <- decision_tree(cost_complexity = 1)

  expect_equal(update(expr1, cost_complexity = .1), expr1_exp)
  expect_equal(update(expr2, model = FALSE), expr2_exp)
  expect_equal(update(expr3, cost_complexity = 1, fresh = TRUE), expr3_exp)

  param_tibb <- tibble::tibble(cost_complexity = 0.1, min_n = 1)
  param_list <- as.list(param_tibb)

  expr1_updated <- update(expr1, param_tibb)
  expect_equal(expr1_updated$args$cost_complexity, 0.1)
  expect_equal(expr1_updated$args$min_n, 1)
  expect_equal(expr1_updated$eng_args$model, rlang::quo(FALSE))

  expr1_updated_lst <- update(expr1, param_list)
  expect_equal(expr1_updated_lst$args$cost_complexity, 0.1)
  expect_equal(expr1_updated_lst$args$min_n, 1)
  expect_equal(expr1_updated_lst$eng_args$model, rlang::quo(FALSE))
})

test_that('bad input', {
  expect_error(decision_tree(mode = "bogus"))
  expect_error({
    bt <- decision_tree(cost_complexity = -1) %>% set_engine("rpart")
    fit(bt, class ~ ., hpc)
  })
  expect_error({
    bt <- decision_tree(min_n = 0)  %>% set_engine("rpart")
    fit(bt, class ~ ., hpc)
  })
  expect_error(translate(decision_tree(), engine = NULL))
  expect_error(translate(decision_tree(formula = y ~ x)))
})

# ------------------------------------------------------------------------------

test_that('argument checks for data dimensions', {

  data(penguins, package = "modeldata")
  penguins <- na.omit(penguins)

  spec <-
    decision_tree(min_n = 1000) %>%
    set_engine("rpart") %>%
    set_mode("regression")

  expect_warning(
    f_fit  <- spec %>% fit(body_mass_g ~ ., data = penguins),
    "1000 samples were requested but there were 333 rows in the data. 333 will be used."
  )
  expect_warning(
    xy_fit <- spec %>% fit_xy(x = penguins[, -6], y = penguins$body_mass_g),
    "1000 samples were requested but there were 333 rows in the data. 333 will be used."
  )

  expect_equal(f_fit$fit$control$minsplit,  nrow(penguins))
  expect_equal(xy_fit$fit$control$minsplit, nrow(penguins))

  spec <-
    decision_tree(min_n = 1000) %>%
    set_engine("spark") %>%
    set_mode("regression")

  args <- translate(spec)$method$fit$args
  expect_equal(args$min_instances_per_node,  rlang::expr(min_rows(1000, x)))

})
