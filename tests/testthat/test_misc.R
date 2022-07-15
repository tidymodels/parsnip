# checking for multi_predict
hpc <- hpc_data[1:150, c(2:5, 8)]

test_that('parsnip objects', {

  lm_idea <- linear_reg() %>% set_engine("lm")
  expect_false(has_multi_predict(lm_idea))

  lm_fit <- fit(lm_idea, mpg ~ ., data = mtcars)
  expect_false(has_multi_predict(lm_fit))
  expect_false(has_multi_predict(extract_fit_engine(lm_fit)))
  expect_error(
    multi_predict(lm_fit, mtcars),
    "No `multi_predict` method exists"
  )

  mars_fit <-
    mars(mode = "regression") %>%
    set_engine("earth") %>%
    fit(mpg ~ ., data = mtcars)
  expect_true(has_multi_predict(mars_fit))
  expect_false(has_multi_predict(extract_fit_engine(mars_fit)))
  expect_error(
    multi_predict(extract_fit_engine(mars_fit), mtcars),
    "No `multi_predict` method exists"
  )

})

test_that('other objects', {

  expect_false(has_multi_predict(NULL))
  expect_false(has_multi_predict(NA))

})

# ------------------------------------------------------------------------------

test_that('S3 method dispatch/registration', {

  expect_error(
    res <-
      null_model() %>%
      set_engine("parsnip") %>%
      set_mode("regression") %>%
      fit(mpg ~ ., data = mtcars) %>%
      tidy(),
    regex = NA
  )
  expect_true(tibble::is_tibble(res))

  expect_error(
    res <-
      null_model() %>%
      set_engine("parsnip") %>%
      set_mode("classification") %>%
      fit(class ~ ., data = hpc) %>%
      tidy(),
    regex = NA
  )
  expect_true(tibble::is_tibble(res))

})

# ------------------------------------------------------------------------------

test_that('control class', {
  x <- linear_reg() %>% set_engine("lm")
  ctrl <- control_parsnip()
  class(ctrl) <- c("potato", "chair")
  expect_error(
    fit(x, mpg ~ ., data = mtcars, control = ctrl),
    "The 'control' argument should have class 'control_parsnip'"
  )
  expect_error(
    fit_xy(x, x = mtcars[, -1], y = mtcars$mpg, control = ctrl),
    "The 'control' argument should have class 'control_parsnip'"
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
  expect_snapshot(
    bag_tree() %>%
      set_engine("rpart")
  )
})

test_that('set_engine works as a generic', {
  expect_snapshot(error = TRUE,
    set_engine(mtcars, "rpart")
  )
})
