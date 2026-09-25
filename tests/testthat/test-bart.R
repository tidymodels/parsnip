test_that("check_args() works", {
  # Here for completeness, no checking is done
  expect_true(TRUE)
})

test_that("dbarts engine allows case weights (#761)", {
  for (mode in c("regression", "classification")) {
    spec <- bart(mode = mode, engine = "dbarts")
    expect_in("weights", names(translate(spec)$method$fit$args))
  }
})

test_that("case weights are passed to dbarts (#761)", {
  skip_if_not_installed("dbarts")

  dat <- mtcars[, c("mpg", "vs", "disp", "hp")]
  dat$vs <- factor(dat$vs)
  wts <- rep(c(1, 100), each = 16)
  spec <- bart(trees = 5, engine = "dbarts")

  reg_fit <- spec |>
    set_mode("regression") |>
    fit(mpg ~ disp + hp, data = dat, case_weights = importance_weights(wts))
  expect_equal(reg_fit$fit$fit$data@weights, wts)

  cls_fit <- spec |>
    set_mode("classification") |>
    fit(vs ~ disp + hp, data = dat, case_weights = importance_weights(wts))
  expect_equal(cls_fit$fit$fit$data@weights, wts)
})

test_that("classification intervals use each observation's own bounds", {
  skip_if_not_installed("dbarts")
  # Issue 1407

  dat <- iris[iris$Species != "virginica", ]
  dat$Species <- droplevels(dat$Species)
  new_data <- dat[c(1:3, 51:53), ]

  set.seed(83156)
  cls_fit <- bart(trees = 5) |>
    set_engine("dbarts") |>
    set_mode("classification") |>
    fit(Species ~ ., data = dat)

  res <- predict(cls_fit, new_data, type = "conf_int")
  expect_named(
    res,
    c(
      ".pred_lower_setosa",
      ".pred_lower_versicolor",
      ".pred_upper_setosa",
      ".pred_upper_versicolor"
    )
  )

  # `type = "ev"` evaluates the stored trees, so this draws no new samples
  post <- predict(extract_fit_engine(cls_fit), new_data, type = "ev")
  bnds <- apply(post, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)

  expect_equal(res$.pred_lower_versicolor, unname(bnds[1, ]))
  expect_equal(res$.pred_upper_versicolor, unname(bnds[2, ]))
  expect_equal(res$.pred_lower_setosa, unname(1 - bnds[2, ]))
  expect_equal(res$.pred_upper_setosa, unname(1 - bnds[1, ]))

  expect_all_true(res$.pred_lower_setosa <= res$.pred_upper_setosa)
  expect_all_true(res$.pred_lower_versicolor <= res$.pred_upper_versicolor)
})

test_that("regression intervals are unaffected", {
  skip_if_not_installed("dbarts")
  # Issue 1407

  set.seed(83156)
  reg_fit <- bart(trees = 5) |>
    set_engine("dbarts") |>
    set_mode("regression") |>
    fit(mpg ~ ., data = mtcars)

  res <- predict(reg_fit, mtcars[1:5, ], type = "conf_int")
  expect_named(res, c(".pred_lower", ".pred_upper"))
  expect_all_true(res$.pred_lower <= res$.pred_upper)
})

test_that("classification probabilities are named from the outcome levels", {
  skip_if_not_installed("dbarts")
  # Issue 1407

  dat <- iris[iris$Species != "virginica", ]
  dat$Species <- droplevels(dat$Species)

  set.seed(83156)
  cls_fit <- bart(trees = 5) |>
    set_engine("dbarts") |>
    set_mode("classification") |>
    fit(Species ~ ., data = dat)

  # guards against `$` partial matching on the `lvl` element
  withr::local_options(warnPartialMatchDollar = TRUE)

  for (pred_type in c("prob", "class", "conf_int", "pred_int")) {
    expect_no_condition(predict(cls_fit, dat[c(1, 51), ], type = pred_type))
  }

  expect_named(
    predict(cls_fit, dat[c(1, 51), ], type = "prob"),
    c(".pred_setosa", ".pred_versicolor")
  )
})

test_that("classification requires exactly two outcome levels", {
  skip_if_not_installed("dbarts")

  spec <- bart(trees = 5) |>
    set_engine("dbarts") |>
    set_mode("classification")

  # unused levels: the fix is `droplevels()`, so the message says so
  unused_last <- iris[iris$Species != "virginica", ]
  expect_snapshot(error = TRUE, fit(spec, Species ~ ., data = unused_last))

  # an unused *first* level previously gave dbarts codes of 1 and 2 rather
  # than 0 and 1, so the returned "probabilities" were not on [0, 1]
  unused_first <- iris[iris$Species != "setosa", ]
  unused_first$Species <- factor(
    unused_first$Species,
    levels = levels(iris$Species)
  )
  expect_snapshot(error = TRUE, fit(spec, Species ~ ., data = unused_first))

  # genuinely multiclass: `droplevels()` would not help, so it is not suggested
  expect_snapshot(error = TRUE, fit(spec, Species ~ ., data = iris))

  # a clean binary outcome is unaffected, and its probabilities are valid
  binary <- unused_last
  binary$Species <- droplevels(binary$Species)
  set.seed(83156)
  cls_fit <- fit(spec, Species ~ ., data = binary)
  probs <- predict(cls_fit, binary[c(1, 51), ], type = "prob")
  expect_all_true(unlist(probs) >= 0 & unlist(probs) <= 1)

  # regression is unaffected by the check
  expect_no_error(
    bart(trees = 5) |>
      set_engine("dbarts") |>
      set_mode("regression") |>
      fit(mpg ~ ., data = mtcars)
  )
})
