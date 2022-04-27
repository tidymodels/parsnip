hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('updating', {
  expr1 <- decision_tree() %>% set_engine("rpart", model = FALSE)
  expr2 <- decision_tree(cost_complexity = tune()) %>% set_engine("rpart", model = tune())
  expr3 <- decision_tree(cost_complexity = 1, min_n = tune())

  param_tibb <- tibble::tibble(cost_complexity = 0.1, min_n = 1)
  param_list <- as.list(param_tibb)

  expect_snapshot(expr1 %>% update(cost_complexity = .1))
  expect_snapshot(expr1 %>% update(param_tibb))
  expect_snapshot(expr1 %>% update(param_list))
  expect_snapshot(expr2 %>% update(model = FALSE))
  expect_snapshot(expr3 %>% update(cost_complexity = 1, fresh = TRUE))
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

  expect_equal(extract_fit_engine(f_fit)$control$minsplit,  nrow(penguins))
  expect_equal(extract_fit_engine(xy_fit)$control$minsplit, nrow(penguins))

  spec <-
    decision_tree(min_n = 1000) %>%
    set_engine("spark") %>%
    set_mode("regression")

  args <- translate(spec)$method$fit$args
  expect_equal(args$min_instances_per_node,  rlang::expr(min_rows(1000, x)))

})
