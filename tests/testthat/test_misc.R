
# ------------------------------------------------------------------------------

context("checking for multi_predict")

test_that('parsnip objects', {

  lm_idea <- linear_reg() %>% set_engine("lm")
  expect_false(has_multi_pred(lm_idea))

  lm_fit <- fit(lm_idea, mpg ~ ., data = mtcars)
  expect_false(has_multi_pred(lm_fit))
  expect_false(has_multi_pred(lm_fit$fit))

  mars_fit <-
    mars(mode = "regression") %>%
    set_engine("earth") %>%
    fit(mpg ~ ., data = mtcars)
  expect_true(has_multi_pred(mars_fit))
  expect_false(has_multi_pred(mars_fit$fit))
})

test_that('other objects', {

  expect_false(has_multi_pred(NULL))
  expect_false(has_multi_pred(NA))

})

# ------------------------------------------------------------------------------

