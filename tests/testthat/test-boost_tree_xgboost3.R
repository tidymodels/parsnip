skip_if_not_installed("xgboost", minimum_version = "3.0.0")

make_cat_data <- function(n = 200) {
  withr::with_seed(2864, {
    dat <- data.frame(
      a = factor(sample(letters[1:5], n, replace = TRUE)),
      b = rnorm(n)
    )
  })
  dat$y <- as.numeric(dat$a %in% c("a", "c")) * 2 + dat$b
  dat$cls <- factor(ifelse(dat$y > 1, "hi", "lo"))
  dat$cls3 <- factor(cut(dat$y, c(-Inf, 0, 2, Inf), labels = c("l", "m", "h")))
  dat
}

xgb3_model_json <- function(x) {
  rawToChar(xgboost::xgb.save.raw(x$fit, raw_format = "json"))
}

test_that("xgboost3 passes factors as categorical features", {
  skip_on_cran()
  dat <- make_cat_data()
  spec <- boost_tree(trees = 10) |>
    set_engine("xgboost3") |>
    set_mode("regression")

  form_fit <- fit(spec, y ~ a + b, data = dat)
  xy_fit <- fit_xy(spec, x = dat[c("a", "b")], y = dat$y)

  expect_s3_class(form_fit$fit, "xgb.Booster")
  expect_match(
    xgb3_model_json(form_fit),
    '"feature_types":["c","float"]',
    fixed = TRUE
  )
  expect_match(
    xgb3_model_json(xy_fit),
    '"feature_types":["c","float"]',
    fixed = TRUE
  )
  expect_identical(form_fit$preproc$ptype, dat[0, c("a", "b")])
})

test_that("xgboost3 predictions use the training factor levels", {
  skip_on_cran()
  dat <- make_cat_data()
  spec <- boost_tree(trees = 10) |>
    set_engine("xgboost3") |>
    set_mode("regression")
  xy_fit <- fit_xy(spec, x = dat[c("a", "b")], y = dat$y)
  form_fit <- fit(spec, y ~ a + b, data = dat)

  same_levels <- data.frame(
    a = factor(c("c", "b"), levels = letters[1:5]),
    b = 0
  )
  reordered <- data.frame(a = factor(c("c", "b"), levels = c("c", "b")), b = 0)
  as_character <- data.frame(a = c("c", "b"), b = 0)

  expected <- predict(xy_fit, same_levels)
  expect_identical(predict(xy_fit, reordered), expected)
  expect_identical(predict(xy_fit, as_character), expected)
  expect_identical(predict(form_fit, reordered), predict(form_fit, same_levels))
})

test_that("xgboost3 converts novel levels to missing values", {
  skip_on_cran()
  dat <- make_cat_data()
  xy_fit <- boost_tree(trees = 10) |>
    set_engine("xgboost3") |>
    set_mode("regression") |>
    fit_xy(x = dat[c("a", "b")], y = dat$y)

  expect_snapshot(
    res <- predict(xy_fit, data.frame(a = c("new", "c"), b = 0))
  )
  expect_identical(
    res,
    predict(
      xy_fit,
      data.frame(a = factor(c(NA, "c"), levels = letters[1:5]), b = 0)
    )
  )
})

test_that("xgboost3 classification, validation, and multi_predict()", {
  skip_on_cran()
  dat <- make_cat_data()
  spec <- boost_tree(trees = 10) |>
    set_engine("xgboost3", validation = 0.2) |>
    set_mode("classification")

  bin_fit <- withr::with_seed(5513, fit(spec, cls ~ a + b, data = dat))
  multi_fit <- withr::with_seed(5513, fit(spec, cls3 ~ a + b, data = dat))

  bin_prob <- predict(bin_fit, dat[1:5, ], type = "prob")
  multi_prob <- predict(multi_fit, dat[1:5, ], type = "prob")
  expect_named(bin_prob, c(".pred_hi", ".pred_lo"))
  expect_named(multi_prob, c(".pred_l", ".pred_m", ".pred_h"))
  expect_s3_class(predict(bin_fit, dat[1:5, ])$.pred_class, "factor")

  reordered <- dat[1:5, ]
  reordered$a <- factor(as.character(reordered$a), levels = rev(letters[1:5]))
  mp <- multi_predict(bin_fit, reordered, trees = c(3, 10), type = "prob")
  expect_identical(nrow(mp), 5L)
  expect_equal(
    purrr::map_dbl(mp$.pred, \(x) x$.pred_hi[x$trees == 10]),
    bin_prob$.pred_hi
  )
})

test_that("xgboost3 quantile regression", {
  skip_on_cran()
  dat <- make_cat_data()
  qnt_fit <- boost_tree(trees = 10) |>
    set_engine("xgboost3") |>
    set_mode("quantile regression", quantile_levels = c(0.2, 0.8)) |>
    fit(y ~ a + b, data = dat)

  res <- predict(qnt_fit, dat[1:3, ])
  expect_named(res, ".pred_quantile")
  expect_identical(nrow(res), 3L)
})

test_that("xgboost3 works without factor predictors", {
  skip_on_cran()
  dat <- make_cat_data()
  spec <- boost_tree(trees = 10) |>
    set_engine("xgboost3") |>
    set_mode("regression")

  num_fit <- fit(spec, y ~ b, data = dat)
  expect_identical(nrow(predict(num_fit, dat[1:3, ])), 3L)

  sparse_x <- Matrix::Matrix(as.matrix(dat["b"]), sparse = TRUE)
  sparse_fit <- fit_xy(spec, x = sparse_x, y = dat$y)
  expect_identical(nrow(predict(sparse_fit, sparse_x[1:3, , drop = FALSE])), 3L)
})

test_that("xgboost3 rejects the exact tree method with factors", {
  skip_on_cran()
  dat <- make_cat_data()
  spec <- boost_tree(trees = 10) |>
    set_engine("xgboost3", tree_method = "exact") |>
    set_mode("regression")

  expect_snapshot(error = TRUE, fit(spec, y ~ a + b, data = dat))
})
