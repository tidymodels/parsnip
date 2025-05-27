skip_if_not_installed("modeldata")

# checking for multi_predict
hpc <- hpc_data[1:150, c(2:5, 8)]

test_that('parsnip objects', {
  skip_if_not_installed("earth")

  lm_idea <- linear_reg() |> set_engine("lm")
  expect_false(has_multi_predict(lm_idea))
  expect_snapshot(error = TRUE, predict(lm_idea, mtcars))

  lm_fit <- fit(lm_idea, mpg ~ ., data = mtcars)
  expect_false(has_multi_predict(lm_fit))
  expect_false(has_multi_predict(extract_fit_engine(lm_fit)))
  expect_snapshot(error = TRUE, multi_predict(lm_fit, mtcars))

  mars_fit <-
    mars(mode = "regression") |>
    set_engine("earth") |>
    fit(mpg ~ ., data = mtcars)
  expect_true(has_multi_predict(mars_fit))
  expect_false(has_multi_predict(extract_fit_engine(mars_fit)))
  expect_snapshot(
    error = TRUE,
    multi_predict(extract_fit_engine(mars_fit), mtcars)
  )

})

test_that('other objects', {

  expect_false(has_multi_predict(NULL))
  expect_false(has_multi_predict(NA))

})

# ------------------------------------------------------------------------------

test_that('S3 method dispatch/registration', {

  expect_no_condition(
    res <-
      null_model() |>
      set_engine("parsnip") |>
      set_mode("regression") |>
      fit(mpg ~ ., data = mtcars) |>
      tidy()
  )
  expect_true(tibble::is_tibble(res))

  expect_no_condition(
    res <-
      null_model() |>
      set_engine("parsnip") |>
      set_mode("classification") |>
      fit(class ~ ., data = hpc) |>
      tidy()
  )
  expect_true(tibble::is_tibble(res))

})

# ------------------------------------------------------------------------------
test_that("combine_words helper works", {
  expect_snapshot(combine_words(1))
  expect_snapshot(combine_words(1:2))
  expect_snapshot(combine_words(1:3))
  expect_snapshot(combine_words(1:4))
})

# ------------------------------------------------------------------------------

test_that('control class', {
  x <- linear_reg() |> set_engine("lm")
  ctrl <- control_parsnip()
  class(ctrl) <- c("potato", "chair")
  # This doesn't error anymore because `condense_control()` doesn't care about
  # classes, it cares about elements
  expect_no_condition(
    fit(x, mpg ~ ., data = mtcars, control = ctrl)
  )
  expect_no_condition(
    fit_xy(x, x = mtcars[, -1], y = mtcars$mpg, control = ctrl)
  )
})

# ------------------------------------------------------------------------------

test_that('correct mtry', {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")
  f_1 <- Sale_Price ~ Longitude + Latitude + Year_Built
  f_2 <- Sale_Price ~ .
  f_3 <- cbind(wt, mpg) ~ .

  expect_equal(max_mtry_formula(2, f_1, ames), 2)
  expect_equal(max_mtry_formula(5, f_1, ames), 3)
  expect_equal(max_mtry_formula(0, f_1, ames), 1)

  expect_equal(max_mtry_formula(2000, f_2, ames), ncol(ames) - 1)
  expect_equal(max_mtry_formula(2, f_2, ames), 2)

  expect_equal(max_mtry_formula(200, f_3, data = mtcars), ncol(mtcars) - 2)

})

# ----------------------------------------------------------------------------

test_that('model type functions message informatively with unknown implementation', {
  # one possible extension --------------------------------------------------
  # known engine, mode
  expect_snapshot(
    bag_tree() |>
      set_engine("rpart") |>
      set_mode("regression")
  )

  # known, uniquely identifying mode
  expect_snapshot(
    bag_tree() |>
      set_mode("censored regression")
  )

  # two possible extensions -------------------------------------------------
  # all default / unknown
  expect_snapshot(
    bag_tree()
  )

  # extension-ambiguous engine
  expect_snapshot(
    bag_tree() |>
      set_engine("rpart")
  )
})

test_that('missing implementation checks prompt conservatively with old objects', {
  # #793 introduced the `user_specified_engine` and `user_specified_mode`
  # slots to parsnip model spec objects. model types defined in external
  # extension packages, as well as model specs generated before parsnip 1.0.2,
  # will not have this slot. ensure that these messages/errors aren't
  # erroneously introduced when that's the case
  #
  # further tests in tidymodels/extratests@53
  bt <-
    bag_tree() |>
    set_engine("rpart") |>
    set_mode("regression")

  bt$user_specified_mode <- NULL
  bt$user_specified_engine <- NULL

  expect_snapshot(bt)
})

test_that('arguments can be passed to model spec inside function', {
  skip_if_not_installed("kknn")
  f <- function(k = 5) {
    nearest_neighbor(mode = "regression", neighbors = k) |>
      fit(mpg ~ ., data = mtcars)
  }

  exp_res <- nearest_neighbor(mode = "regression", neighbors = 5) |>
    fit(mpg ~ ., data = mtcars)

  expect_no_condition(fun_res <- f())

  expect_equal(exp_res$fit[-c(8, 9)], fun_res$fit[-c(8, 9)])
})


test_that('set_engine works as a generic', {
  expect_snapshot(error = TRUE,
                  set_engine(mtcars, "rpart")
  )

})

test_that('check_for_newdata points out correct context', {
  fn <- function(...) {check_for_newdata(...); invisible()}
  expect_snapshot(error = TRUE,
                  fn(newdata = "boop!")
  )
})

test_that('check_outcome works as expected', {
  reg_spec <- linear_reg()

  expect_no_error(
    check_outcome(1:2, reg_spec)
  )

  expect_no_error(
    check_outcome(mtcars, reg_spec)
  )

  expect_snapshot(
    error = TRUE,
    check_outcome(NULL, reg_spec)
  )

  expect_snapshot(
    error = TRUE,
    check_outcome(tibble::new_tibble(list(), nrow = 10), reg_spec)
  )

  expect_snapshot(
    error = TRUE,
    fit(reg_spec, ~ mpg, mtcars)
  )

  expect_snapshot(
    error = TRUE,
    fit_xy(reg_spec, data.frame(x = 1:5), y = NULL)
  )

  class_spec <- logistic_reg()

  expect_no_error(
    check_outcome(factor(1:2), class_spec)
  )

  expect_no_error(
    check_outcome(lapply(mtcars, as.factor), class_spec)
  )

  expect_snapshot(
    error = TRUE,
    check_outcome(NULL, class_spec)
  )

  expect_snapshot(
    error = TRUE,
    check_outcome(tibble::new_tibble(list(), nrow = 10), class_spec)
  )

  expect_snapshot(
    error = TRUE,
    fit(class_spec, ~ mpg, mtcars)
  )

  # Fake specification to avoid having to load {censored}
  cens_spec <- logistic_reg()
  cens_spec$mode <- "censored regression"

  expect_no_error(
    check_outcome(survival::Surv(1, 1), cens_spec)
  )

  expect_snapshot(
    error = TRUE,
    check_outcome(1:2, cens_spec)
  )
})

# ------------------------------------------------------------------------------

test_that('obtaining prediction columns', {
  skip_if_not_installed("modeldata")
  data(two_class_dat, package = "modeldata")

  ### classification
  lr_fit <- logistic_reg() |> fit(Class ~ ., data = two_class_dat)
  expect_equal(
    .get_prediction_column_names(lr_fit),
    list(estimate = ".pred_class",
         probabilities = c(".pred_Class1", ".pred_Class2"))
  )
  expect_equal(
    .get_prediction_column_names(lr_fit, syms = TRUE),
    list(estimate = list(quote(.pred_class)),
         probabilities = list(quote(.pred_Class1), quote(.pred_Class2)))
  )

  ### regression
  ols_fit <- linear_reg() |> fit(mpg ~ ., data = mtcars)
  expect_equal(
    .get_prediction_column_names(ols_fit),
    list(estimate = ".pred",
         probabilities = character(0))
  )
  expect_equal(
    .get_prediction_column_names(ols_fit, syms = TRUE),
    list(estimate = list(quote(.pred)),
         probabilities = list())
  )

  ### censored regression
  # in extratests

  ### bad input
  expect_snapshot(
    .get_prediction_column_names(1),
    error = TRUE
  )

  unk_fit <- ols_fit
  unk_fit$spec$mode <- "Depeche"
  expect_snapshot(
    .get_prediction_column_names(unk_fit),
    error = TRUE
  )

})


# ------------------------------------------------------------------------------

# https://github.com/tidymodels/parsnip/issues/1229
test_that('register local models', {
  set_new_model("my_model")
  set_model_mode(model = "my_model", mode = "regression")
  set_model_engine(
    "my_model",
    mode = "regression",
    eng = "my_engine"
  )

  my_model <-
    function(mode = "regression") {
      new_model_spec(
        "my_model",
        args = list(),
        eng_args = NULL,
        mode = mode,
        method = NULL,
        engine = NULL
      )
    }

  set_fit(
    model = "my_model",
    eng = "my_engine",
    mode = "regression",
    value = list(
      interface = "matrix",
      protect = c("formula", "data"),
      func = c(fun = "my_model_fun"),
      defaults = list()
    )
  )

  expect_snapshot(my_model() |> translate("my_engine"))
})

