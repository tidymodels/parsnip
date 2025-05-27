skip_if_not_installed("dials")

test_that('extract', {
  x <- linear_reg() |> set_engine("lm") |> fit(mpg ~ ., data = mtcars)
  x_no_spec <- x
  x_no_spec$spec <- NULL
  x_no_fit <- x
  x_no_fit$fit <- NULL

  expect_true(inherits(extract_spec_parsnip(x), "model_spec"))
  expect_true(inherits(extract_fit_engine(x), "lm"))

  expect_snapshot(error = TRUE, extract_spec_parsnip(x_no_spec))
  expect_snapshot(error = TRUE, extract_fit_engine(x_no_fit))
})

# ------------------------------------------------------------------------------

test_that('extract parameter set from model with no parameters', {
  skip_on_covr()

  lm_model <- linear_reg() |> set_engine("lm")

  lm_info <- extract_parameter_set_dials(lm_model)
  check_parameter_set_tibble(lm_info)
  expect_equal(nrow(lm_info), 0)
})

test_that('extract parameter set from model with main and engine parameters', {
  skip_on_covr()

  bst_model <-
    boost_tree(mode = "classification", trees = hardhat::tune("funky name \n")) |>
    set_engine("C5.0", rules = hardhat::tune(), noGlobalPruning = TRUE)

  c5_info <- extract_parameter_set_dials(bst_model)
  check_parameter_set_tibble(c5_info)
  expect_equal(nrow(c5_info), 2)
  expect_true(all(c5_info$source == "model_spec"))
  expect_true(all(c5_info$component == "boost_tree"))
  expect_equal(c5_info$component_id, c("main", "engine"))
  nms <- c("trees", "rules")
  expect_equal(c5_info$name, nms)
  ids <- c("funky name \n", "rules")
  expect_equal(c5_info$id, ids)

  expect_equal(c5_info$object[[1]], dials::trees(c(1, 100)))
  expect_equal(c5_info$object[[2]], NA)
})

test_that('extract parameter set from model with no loaded implementation', {
  bt_mod <- bag_tree(min_n = tune()) |>
    set_mode("regression")

  expect_snapshot(error = TRUE, extract_parameter_set_dials(bt_mod))
  expect_snapshot(error = TRUE, extract_parameter_dials(bt_mod, parameter = "min_n"))
})

# ------------------------------------------------------------------------------

test_that('extract single parameter from model with no parameters', {
  skip_on_covr()

  lm_model <- linear_reg() |> set_engine("lm")

  expect_snapshot(
    error = TRUE,
    extract_parameter_dials(lm_model, parameter = "none there")
  )
})

test_that('extract single parameter from model with main and engine parameters', {
  skip_on_covr()

  bst_model <-
    boost_tree(mode = "classification", trees = hardhat::tune("funky name \n")) |>
    set_engine("C5.0", rules = hardhat::tune(), noGlobalPruning = TRUE)

  expect_equal(
    hardhat::extract_parameter_dials(bst_model, parameter = "funky name \n"),
    dials::trees(c(1, 100))
  )
  expect_equal(
    extract_parameter_dials(bst_model, parameter = "rules"),
    NA
  )
})

test_that("extract_parameter_dials doesn't error if namespaced args are used", {
  skip_on_covr()

  bst_model <-
    logistic_reg(mode = "classification", penalty = hardhat::tune()) |>
      set_engine("glmnet", family = stats::gaussian("log"))

  expect_no_condition(
    extract_parameter_dials(bst_model, parameter = "penalty")
  )
})

test_that("extract_fit_time() works", {
  lm_fit <- linear_reg() |> fit(mpg ~ ., data = mtcars)

  res <- extract_fit_time(lm_fit)

  expect_true(is_tibble(res))
  expect_identical(names(res), c("stage_id", "elapsed"))
  expect_identical(res$stage_id, "linear_reg")
  expect_true(is.double(res$elapsed))
  expect_true(res$elapsed >= 0)

  lm_fit$elapsed$elapsed <- NULL

  expect_snapshot(
    error = TRUE,
    extract_fit_time(lm_fit)
  )
})
