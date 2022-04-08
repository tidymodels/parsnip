# checking for multi_predict
hpc <- hpc_data[1:150, c(2:5, 8)]

test_that('parsnip objects', {

  lm_idea <- linear_reg() %>% set_engine("lm")
  expect_false(has_multi_predict(lm_idea))

  lm_fit <- fit(lm_idea, mpg ~ ., data = mtcars)
  expect_false(has_multi_predict(lm_fit))
  expect_false(has_multi_predict(lm_fit$fit))
  expect_error(
    multi_predict(lm_fit, mtcars),
    "No `multi_predict` method exists"
  )

  mars_fit <-
    mars(mode = "regression") %>%
    set_engine("earth") %>%
    fit(mpg ~ ., data = mtcars)
  expect_true(has_multi_predict(mars_fit))
  expect_false(has_multi_predict(mars_fit$fit))
  expect_error(
    multi_predict(mars_fit$fit, mtcars),
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


