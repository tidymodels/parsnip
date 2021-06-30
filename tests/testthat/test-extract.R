
context("model extraction")

# ------------------------------------------------------------------------------

test_that('extract', {
  x <- linear_reg() %>% set_engine("lm") %>% fit(mpg ~ ., data = mtcars)
  x_no_spec <- x
  x_no_spec$spec <- NULL
  x_no_fit <- x
  x_no_fit$fit <- NULL

  expect_true(inherits(extract_spec_parsnip(x), "model_spec"))
  expect_true(inherits(extract_fit_engine(x), "lm"))

  expect_error(extract_spec_parsnip(x_no_spec), "Internal error")
  expect_error(extract_fit_engine(x_no_fit), "Internal error")
})

