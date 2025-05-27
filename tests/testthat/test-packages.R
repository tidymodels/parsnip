
load(test_path("mars_model.RData"))

# ------------------------------------------------------------------------------

test_that('required packages', {
  rlang::local_options(lifecycle_verbosity = "quiet")

  glmn <-
    linear_reg() |>
    set_engine("glmnet")
  expect_equal(required_pkgs(glmn), c("parsnip", "glmnet"))
  expect_equal(required_pkgs(glmn, infra = FALSE), c("glmnet"))

  expect_snapshot(error = TRUE, {
    expect_equal(req_pkgs(glmn), "glmnet")
  })

  lm_fit <-
    linear_reg() |>
    set_engine("lm") |>
    fit(mpg ~ ., data = mtcars)

  expect_equal(required_pkgs(lm_fit), c("parsnip", "stats"))
  expect_equal(required_pkgs(lm_fit, infra = FALSE), c("stats"))

  expect_snapshot(error = TRUE, {
    expect_equal(req_pkgs(lm_fit), "stats")
  })
})

# ------------------------------------------------------------------------------

test_that('missing packages', {
  skip_if_not_installed("earth")

  has_earth <- parsnip:::is_installed("earth")

  if (has_earth) {
    expect_no_condition(predict(mars_model, mtcars[1:3, -1]))

  } else {
    expect_snapshot(error = TRUE, predict(mars_model, mtcars[1:3, -1]))
    expect_true(any(names(sessionInfo()$loadedOnly) == "earth"))
  }
  mars_model$spec$method$libs <- "rootveggie"
  expect_snapshot(error = TRUE, predict(mars_model, mtcars[1:3, -1]))

})

