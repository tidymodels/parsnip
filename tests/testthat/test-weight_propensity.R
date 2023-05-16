test_that("basic functionality", {
  skip_if_not_installed("modeldata")
  library(modeldata)
  library(parsnip)

  silly_wt_fn <- function(.propensity, .exposure = NULL, ...) {
    seq(1, 2, length.out = length(.propensity))
  }

  lr_fit <- fit(logistic_reg(), Class ~ A + B, two_class_dat)

  lr_res1 <- weight_propensity(lr_fit, silly_wt_fn, data = two_class_dat)
  expect_s3_class(lr_res1, "tbl_df")
  expect_true(all(names(lr_res1) %in% c(names(two_class_dat), ".wts")))
  expect_equal(lr_res1$.wts, importance_weights(seq(1, 2, length.out = nrow(two_class_dat))))
})

test_that("errors informatively with bad input", {
  skip_if_not_installed("modeldata")
  library(modeldata)
  library(parsnip)

  silly_wt_fn <- function(.propensity, .exposure = NULL, ...) {
    seq(1, 2, length.out = length(.propensity))
  }

  # bad `object`
  spec <- logistic_reg()

  expect_snapshot(
    error = TRUE,
    weight_propensity(spec, silly_wt_fn, data = two_class_dat)
  )

  expect_snapshot(
    error = TRUE,
    weight_propensity("boop", silly_wt_fn, data = two_class_dat)
  )

  # bad `wt_fn`
  spec_fit <- fit(spec, Class ~ A + B, data = two_class_dat)

  expect_snapshot(
    error = TRUE,
    weight_propensity(spec_fit, two_class_dat)
  )

  expect_snapshot(
    error = TRUE,
    weight_propensity(spec_fit, "boop", data = two_class_dat)
  )

  expect_snapshot(
    error = TRUE,
    weight_propensity(spec_fit, function(...) {-1L}, data = two_class_dat)
  )

  # bad `data`
  expect_snapshot(
    error = TRUE,
    weight_propensity(spec_fit, silly_wt_fn)
  )

  expect_snapshot(
    error = TRUE,
    weight_propensity(spec_fit, silly_wt_fn, data = "boop")
  )
})
