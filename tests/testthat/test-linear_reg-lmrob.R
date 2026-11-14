skip_if_not_installed("robustbase")

# ------------------------------------------------------------------------------

test_that("lmrob translates engine arguments", {
  spec <- linear_reg() |> set_engine("lmrob", max.it = 100)
  translated <- translate(spec)
  expect_equal(translated$method$fit$func, c(pkg = "robustbase", fun = "lmrob"))
  expect_equal(rlang::eval_tidy(translated$method$fit$args$max.it), 100)
  expect_true(case_weights_allowed(spec))
  expect_equal(nrow(tunable(linear_reg(engine = "lmrob"))), 0)
})

test_that("lmrob formula fits and predictions match robustbase", {
  spec <- linear_reg() |> set_engine("lmrob")
  fitted <- withr::with_seed(
    123,
    fit(spec, mpg ~ wt * factor(cyl), data = mtcars)
  )
  reference <- withr::with_seed(
    123,
    robustbase::lmrob(mpg ~ wt * factor(cyl), data = mtcars)
  )
  expect_s3_class(fitted$fit, "lmrob")
  expect_equal(coef(fitted$fit), coef(reference))
  expected <- unname(predict(reference, mtcars[1:5, ]))
  predictions <- predict(fitted, mtcars[1:5, ])
  expect_s3_class(predictions, "tbl_df")
  expect_named(predictions, ".pred")
  expect_equal(predictions$.pred, expected)
  expect_equal(unname(predict(fitted, mtcars[1:5, ], type = "raw")), expected)
})

test_that("lmrob accepts data frame and matrix xy interfaces", {
  spec <- linear_reg() |> set_engine("lmrob", max.it = 100)
  x <- mtcars[, c("wt", "hp")]
  reference <- withr::with_seed(123, fit(spec, mpg ~ wt + hp, data = mtcars))
  xy <- withr::with_seed(123, fit_xy(spec, x = x, y = mtcars$mpg))
  matrix_xy <- withr::with_seed(
    123, fit_xy(spec, x = as.matrix(x), y = mtcars$mpg)
  )
  expect_equal(coef(xy$fit), coef(reference$fit))
  expect_equal(coef(matrix_xy$fit), coef(reference$fit))
  expect_equal(predict(xy, x[1:5, ]), predict(reference, x[1:5, ]))
  expect_equal(
    predict(matrix_xy, as.matrix(x[1:5, ])), predict(reference, x[1:5, ])
  )
  expect_equal(xy$fit$control$max.it, 100)
})

test_that("lmrob confidence and prediction intervals match robustbase", {
  fitted <- withr::with_seed(
    123,
    fit(linear_reg(engine = "lmrob"), mpg ~ wt + hp, data = mtcars)
  )
  intervals <- c(conf_int = "confidence", pred_int = "prediction")
  for (level in c(0.8, 0.95)) {
    for (type in names(intervals)) {
      reference <- predict(
        fitted$fit, mtcars[1:5, ], interval = intervals[[type]], level = level
      )
      actual <- predict(fitted, mtcars[1:5, ], type = type, level = level)
      expect_named(actual, c(".pred_lower", ".pred_upper"))
      expect_equal(actual$.pred_lower, unname(reference[, "lwr"]))
      expect_equal(actual$.pred_upper, unname(reference[, "upr"]))
    }
  }
})

test_that("lmrob forwards case weights to robustbase", {
  weights <- seq_len(nrow(mtcars)) / nrow(mtcars) + 1
  spec <- linear_reg(engine = "lmrob")
  fitted <- withr::with_seed(
    123,
    fit(spec, mpg ~ wt + hp,
      data = mtcars,
      case_weights = hardhat::importance_weights(weights)
    )
  )
  reference <- withr::with_seed(
    123,
    robustbase::lmrob(mpg ~ wt + hp, data = mtcars, weights = weights)
  )
  expect_equal(coef(fitted$fit), coef(reference))
  expect_equal(unname(fitted$fit$weights), weights)
  expect_equal(
    predict(fitted, mtcars[1:5, ])$.pred,
    unname(predict(reference, mtcars[1:5, ]))
  )
  xy <- withr::with_seed(
    123,
    fit_xy(spec,
      x = mtcars[, c("wt", "hp")], y = mtcars$mpg,
      case_weights = hardhat::importance_weights(weights)
    )
  )
  expect_equal(coef(xy$fit), coef(reference))
})

test_that("lmrob preserves prediction rows with missing predictors", {
  fitted <- withr::with_seed(
    123,
    fit(linear_reg(engine = "lmrob"), mpg ~ wt + hp, data = mtcars)
  )
  new_data <- mtcars[1:5, ]
  new_data$wt[2] <- NA_real_
  actual <- predict(fitted, new_data)
  expect_equal(nrow(actual), 5L)
  expect_equal(which(is.na(actual$.pred)), 2L)
  expect_equal(actual$.pred, unname(predict(fitted$fit, new_data)))
})

test_that("lmrob handles factors and warns on novel levels", {
  data <- mtcars
  data$cyl <- factor(data$cyl)
  spec <- linear_reg(engine = "lmrob")
  fitted <- withr::with_seed(123, fit(spec, mpg ~ wt + cyl, data = data))
  xy <- withr::with_seed(
    123,
    fit_xy(spec, x = data[, c("wt", "cyl")], y = data$mpg)
  )
  expect_equal(predict(xy, data[1:5, ]), predict(fitted, data[1:5, ]))
  new_data <- data[1, ]
  new_data$cyl <- factor("10")
  for (model in list(fitted, xy)) {
    expect_warning(prediction <- predict(model, new_data), "Novel level")
    expect_identical(prediction$.pred, NA_real_)
  }
})

test_that("lmrob raw predictions forward prediction options", {
  fitted <- withr::with_seed(
    123,
    fit(linear_reg(engine = "lmrob"), mpg ~ wt + hp, data = mtcars)
  )
  actual <- predict(
    fitted, mtcars[1:5, ], type = "raw", opts = list(se.fit = TRUE)
  )
  expect_equal(actual, predict(fitted$fit, mtcars[1:5, ], se.fit = TRUE))
})

test_that("lmrob predicts a single row for all prediction types", {
  fitted <- withr::with_seed(
    123,
    fit(linear_reg(engine = "lmrob"), mpg ~ wt + hp, data = mtcars)
  )
  new_data <- mtcars[1, , drop = FALSE]
  numeric <- predict(fitted, new_data)
  reference <- predict(fitted$fit, new_data)
  expect_equal(nrow(numeric), 1L)
  expect_named(numeric, ".pred")
  expect_equal(numeric$.pred, unname(reference))
  expect_equal(predict(fitted, new_data, type = "raw"), reference)

  intervals <- c(conf_int = "confidence", pred_int = "prediction")
  for (type in names(intervals)) {
    actual <- predict(fitted, new_data, type = type, level = 0.95)
    reference <- predict(
      fitted$fit, new_data, interval = intervals[[type]], level = 0.95
    )
    expect_equal(nrow(actual), 1L)
    expect_named(actual, c(".pred_lower", ".pred_upper"))
    expect_equal(
      as.numeric(actual[1, ]), unname(reference[1, c("lwr", "upr")])
    )
  }
})

test_that("lmrob works with formula workflows", {
  skip_if_not_installed("workflows")

  spec <- linear_reg(engine = "lmrob")
  workflow <- workflows::workflow() |>
    workflows::add_formula(mpg ~ wt + hp) |>
    workflows::add_model(spec)
  fitted <- withr::with_seed(123, fit(workflow, data = mtcars))
  reference <- withr::with_seed(123, fit(spec, mpg ~ wt + hp, data = mtcars))
  actual <- predict(fitted, mtcars[1:5, ])
  expect_s3_class(actual, "tbl_df")
  expect_equal(actual, predict(reference, mtcars[1:5, ]))
})

test_that("lmrob works with recipe workflows", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")

  spec <- linear_reg(engine = "lmrob")
  recipe <- recipes::recipe(mpg ~ wt + hp, data = mtcars) |>
    recipes::step_normalize(recipes::all_numeric_predictors())
  workflow <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(spec)
  fitted <- withr::with_seed(123, fit(workflow, data = mtcars))
  baked <- recipes::bake(recipes::prep(recipe), new_data = mtcars)
  reference <- withr::with_seed(123, fit(spec, mpg ~ wt + hp, data = baked))
  actual <- predict(fitted, mtcars[1:5, ])
  expect_s3_class(actual, "tbl_df")
  expect_equal(actual, predict(reference, baked[1:5, ]))
})
