hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('updating', {
  expect_snapshot(
    decision_tree(cost_complexity = .1) %>%
      set_engine("rpart", model = FALSE) %>%
      update(cost_complexity = tune(), model = tune())
  )
})

test_that('bad input', {
  expect_snapshot_error(decision_tree(mode = "bogus"))
  expect_snapshot_error({
    bt <- decision_tree(cost_complexity = -1) %>% set_engine("rpart")
    fit(bt, class ~ ., hpc)
  })
  expect_snapshot_error({
    bt <- decision_tree(min_n = 0)  %>% set_engine("rpart")
    fit(bt, class ~ ., hpc)
  })
  expect_snapshot(
    try(translate(decision_tree(), engine = NULL), silent = TRUE)
  )
})

test_that('rpart_train is stop-deprecated when it ought to be (#1044)', {
  skip_on_cran()

  # once this test fails, transition `rpart_train()` to `deprecate_stop()`
  # and transition this test to fail if `rpart_train()` still exists after a year.
  if (Sys.Date() > "2025-01-01") {
    expect_snapshot(error = TRUE, rpart_train(mpg ~ ., mtcars))
  }
})

# ------------------------------------------------------------------------------

test_that('argument checks for data dimensions', {

  data(penguins, package = "modeldata")
  penguins <- na.omit(penguins)

  spec <-
    decision_tree(min_n = 1000) %>%
    set_engine("rpart") %>%
    set_mode("regression")

  expect_snapshot(
    f_fit  <- spec %>% fit(body_mass_g ~ ., data = penguins)
  )
  expect_snapshot(
    xy_fit <- spec %>% fit_xy(x = penguins[, -6], y = penguins$body_mass_g)
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

test_that("check_args() works", {
  # Here for completeness, no checking is done
  expect_true(TRUE)
})
