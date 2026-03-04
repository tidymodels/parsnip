skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)] |> as.data.frame()

test_that('bad input', {
  expect_snapshot(
    error = TRUE,
    translate(null_model(mode = "regression") |> set_engine())
  )
  expect_snapshot(error = TRUE, translate(null_model() |> set_engine("wat?")))
  expect_warning(
    translate(
      null_model(mode = "regression") |>
        set_engine("parsnip", x = hpc[, 1:3], y = hpc$class)
    ),
    class = "parsnip_protected_arg_warning"
  )
})

# ------------------------------------------------------------------------------

num_pred <- names(hpc)[1:3]
hpc_bad_form <- as.formula(class ~ term)

# ------------------------------------------------------------------------------

test_that('nullmodel execution', {
  expect_no_condition(
    res <- fit(
      null_model(mode = "regression") |> set_engine("parsnip"),
      compounds ~ log(input_fields) + class,
      data = hpc
    )
  )
  expect_no_condition(
    res <- fit(
      null_model(mode = "regression"),
      compounds ~ log(input_fields) + class,
      data = hpc
    )
  )
  expect_no_condition(
    res <- fit_xy(
      null_model(mode = "regression") |> set_engine("parsnip"),
      x = hpc[, num_pred],
      y = hpc$num_pending
    )
  )
  expect_no_condition(
    res <- fit_xy(
      null_model(mode = "regression"),
      x = hpc[, num_pred],
      y = hpc$num_pending
    )
  )

  expect_snapshot(
    error = TRUE,
    res <- fit(
      null_model(mode = "regression") |> set_engine("parsnip"),
      hpc_bad_form,
      data = hpc
    )
  )

  ## multivariate y

  expect_no_condition(
    res <- fit(
      null_model(mode = "regression") |> set_engine("parsnip"),
      cbind(compounds, input_fields) ~ .,
      data = hpc
    )
  )
  expect_no_condition(
    res <- fit(
      null_model(mode = "regression"),
      cbind(compounds, input_fields) ~ .,
      data = hpc
    )
  )
})

test_that('nullmodel prediction', {
  uni_pred <- tibble(.pred = rep(30.1, 5))
  inl_pred <- rep(30.1, 5)
  mw_pred <- tibble(gear = rep(3.6875, 5), carb = rep(2.8125, 5))

  res_xy <- fit_xy(
    null_model(mode = "regression") |> set_engine("parsnip"),
    x = hpc[, num_pred],
    y = hpc$num_pending
  )

  expect_equal(
    uni_pred,
    predict(res_xy, new_data = hpc[1:5, num_pred]),
    tolerance = .01
  )

  res_form <- fit(
    null_model(mode = "regression") |> set_engine("parsnip"),
    num_pending ~ log(compounds) + class,
    data = hpc
  )
  expect_equal(inl_pred, predict(res_form, hpc[1:5, ])$.pred, tolerance = .01)

  # Multivariate y
  res <- fit(
    null_model(mode = "regression") |> set_engine("parsnip"),
    cbind(gear, carb) ~ .,
    data = mtcars
  )

  expect_equal(
    setNames(mw_pred, paste0(".pred_", names(mw_pred))),
    predict(res, mtcars[1:5, ])
  )
})

# ------------------------------------------------------------------------------

test_that('classification', {
  expect_no_condition(
    null_model <- null_model(mode = "classification") |>
      set_engine("parsnip") |>
      fit(class ~ ., data = hpc)
  )
  expect_true(!is.null(null_model$fit))
})

# ------------------------------------------------------------------------------

test_that('null_model printing', {
  expect_snapshot(print(null_model(mode = "classification")))
  expect_snapshot(
    print(
      null_model(mode = "classification") |>
        set_engine("parsnip") |>
        translate()
    )
  )
})

test_that("check_args() works", {
  # Here for completeness, no checking is done
  expect_true(TRUE)
})

# ------------------------------------------------------------------------------

test_that("null_model works with sparse matrix data - regression", {
  skip_if_not_installed("sparsevctrs")

  # Make materialization of sparse vectors throw an error
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates()

  spec <- null_model(mode = "regression") |>
    set_engine("parsnip")

  expect_no_error(
    null_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )

  expect_no_error(
    preds <- predict(null_fit, hotel_data)
  )

  # All predictions should be the mean of the outcome
  expect_true(all(preds$.pred == preds$.pred[1]))
})

test_that("null_model works with sparse matrix data - classification", {
  skip_if_not_installed("sparsevctrs")

  # Make materialization of sparse vectors throw an error
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates()

  # Create a factor outcome for classification
  y_class <- factor(ifelse(
    hotel_data[, 1] > median(hotel_data[, 1]),
    "high",
    "low"
  ))

  spec <- null_model(mode = "classification") |>
    set_engine("parsnip")

  expect_no_error(
    null_fit <- fit_xy(spec, x = hotel_data[, -1], y = y_class)
  )

  expect_no_error(
    preds <- predict(null_fit, hotel_data)
  )

  # All predictions should be the same (most prevalent class)
  expect_true(all(preds$.pred_class == preds$.pred_class[1]))

  expect_no_error(
    probs <- predict(null_fit, hotel_data, type = "prob")
  )

  # All probability predictions should be identical
  expect_true(all(probs$.pred_high == probs$.pred_high[1]))
  expect_true(all(probs$.pred_low == probs$.pred_low[1]))
})

test_that("null_model works with sparse tibble data - regression", {
  skip_if_not_installed("sparsevctrs")

  # Make materialization of sparse vectors throw an error
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  spec <- null_model(mode = "regression") |>
    set_engine("parsnip")

  expect_no_error(
    null_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )

  expect_no_error(
    preds <- predict(null_fit, hotel_data)
  )

  # All predictions should be the mean of the outcome
  expect_true(all(preds$.pred == preds$.pred[1]))
})
