
context("checking for packages")
load(test_path("mars_model.RData"))

# ------------------------------------------------------------------------------

test_that('required packages', {

  glmn <-
    linear_reg() %>%
    set_engine("glmnet")
  expect_equal(req_pkgs(glmn), "glmnet")
  expect_equal(required_pkgs(glmn), c("glmnet", "parsnip"))


  lm_fit <-
    linear_reg() %>%
    set_engine("lm") %>%
    fit(mpg ~ ., data = mtcars)
  expect_equal(req_pkgs(lm_fit), "stats")
  expect_equal(required_pkgs(lm_fit), c("parsnip", "stats"))
})

# ------------------------------------------------------------------------------

test_that('missing packages', {
  has_earth <- parsnip:::is_installed("earth")

  if (has_earth) {
    expect_error(predict(mars_model, mtcars[1:3, -1]), regexp = NA)

  } else {
    expect_error(predict(mars_model, mtcars[1:3, -1]), regexp = "earth")
    expect_true(any(names(sessionInfo()$loadedOnly) == "earth"))
  }
  mars_model$spec$method$libs <- "rootveggie"
  expect_error(predict(mars_model, mtcars[1:3, -1]), regexp = "rootveggie")

})

