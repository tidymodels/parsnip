
# ------------------------------------------------------------------------------

context("checking for multi_predict")

test_that('parsnip objects', {

  lm_idea <- linear_reg() %>% set_engine("lm")
  expect_false(has_multi_predict(lm_idea))

  lm_fit <- fit(lm_idea, mpg ~ ., data = mtcars)
  expect_false(has_multi_predict(lm_fit))
  expect_false(has_multi_predict(lm_fit$fit))

  mars_fit <-
    mars(mode = "regression") %>%
    set_engine("earth") %>%
    fit(mpg ~ ., data = mtcars)
  expect_true(has_multi_predict(mars_fit))
  expect_false(has_multi_predict(mars_fit$fit))
})

test_that('other objects', {

  expect_false(has_multi_predict(NULL))
  expect_false(has_multi_predict(NA))

})

# ------------------------------------------------------------------------------

context("getting y names from terms")

test_that('getting y names from terms', {

  expect_equal(
    parsnip:::terms_y(lm(cbind(mpg, disp)  ~., data = mtcars)$terms),
    c("mpg", "disp")
  )

  expect_equal(
    parsnip:::terms_y(lm(mpg  ~., data = mtcars)$terms),
    "mpg"
  )

  expect_equal(
    parsnip:::terms_y(lm(log(mpg)  ~., data = mtcars)$terms),
    "mpg"
  )

  expect_equal(
    parsnip:::terms_y(terms(  ~., data = mtcars)),
    character(0)
  )


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
      fit(Species ~ ., data = iris) %>%
      tidy(),
    regex = NA
  )
  expect_true(tibble::is_tibble(res))



})


