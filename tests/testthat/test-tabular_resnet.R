test_that("tabular_resnet() creates a model spec with correct defaults", {
  spec <- tabular_resnet()

  expect_s3_class(spec, "tabular_resnet")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "unknown")
  expect_equal(spec$engine, "brulee")
})

test_that("tabular_resnet() accepts its modes", {
  expect_equal(tabular_resnet(mode = "classification")$mode, "classification")
  expect_equal(tabular_resnet(mode = "regression")$mode, "regression")

  expect_snapshot(error = TRUE, tabular_resnet(mode = "quantile regression"))
})

test_that("tabular_resnet() captures arguments as quosures", {
  spec <- tabular_resnet(
    hidden_units = 16L,
    bottleneck_units = 4L,
    residual_at = 2L,
    penalty = 0.01,
    epochs = 50L
  )

  expect_equal(rlang::quo_get_expr(spec$args$hidden_units), 16L)
  expect_equal(rlang::quo_get_expr(spec$args$bottleneck_units), 4L)
  expect_equal(rlang::quo_get_expr(spec$args$residual_at), 2L)
  expect_equal(rlang::quo_get_expr(spec$args$penalty), 0.01)
  expect_equal(rlang::quo_get_expr(spec$args$epochs), 50L)
})

test_that("update.tabular_resnet() updates arguments", {
  spec <- tabular_resnet(hidden_units = 8L, epochs = 50L)

  updated <- update(spec, hidden_units = 32L)
  expect_equal(rlang::quo_get_expr(updated$args$hidden_units), 32L)
  expect_equal(rlang::quo_get_expr(updated$args$epochs), 50L)

  fresh <- update(spec, hidden_units = 32L, fresh = TRUE)
  expect_equal(rlang::quo_get_expr(fresh$args$hidden_units), 32L)
  expect_null(rlang::quo_get_expr(fresh$args$epochs))
})

test_that("check_args.tabular_resnet() validates input values", {
  expect_no_error(
    tabular_resnet(mode = "regression", penalty = 0.1) |> check_args()
  )
  expect_no_error(
    tabular_resnet(mode = "regression", dropout = 0.5) |> check_args()
  )
  expect_no_error(tabular_resnet(mode = "regression") |> check_args())

  expect_snapshot(
    error = TRUE,
    tabular_resnet(mode = "regression", penalty = -1) |> check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_resnet(mode = "regression", mixture = 2) |> check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_resnet(mode = "regression", dropout = 1.5) |> check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_resnet(mode = "regression", stop_iter = 0L) |> check_args()
  )
})

test_that("check_args.tabular_resnet() rejects both penalty and dropout", {
  spec <- tabular_resnet(mode = "regression", penalty = 0.1, dropout = 0.2)
  expect_snapshot(error = TRUE, check_args(spec))
})
