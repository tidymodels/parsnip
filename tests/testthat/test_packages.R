
context("checking for packages")
load(test_path("mars_model.RData"))

# ------------------------------------------------------------------------------

test_that('required packages', {

  expect_error(req_pkgs(linear_reg()), "Please set an engine")

  glmn <-
    linear_reg() %>%
    set_engine("glmnet") %>%
    req_pkgs()
  expect_equal(glmn, "glmnet")

  lm_fit <-
    linear_reg() %>%
    set_engine("lm") %>%
    fit(mpg ~ ., data = mtcars) %>%
    req_pkgs()
  expect_equal(lm_fit, "stats")
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

