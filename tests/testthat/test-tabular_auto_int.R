test_that("tabular_auto_int() creates a model spec with correct defaults", {
  spec <- tabular_auto_int()

  expect_s3_class(spec, "tabular_auto_int")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "unknown")
  expect_equal(spec$engine, "brulee")
})

test_that("tabular_auto_int() accepts its modes", {
  expect_equal(tabular_auto_int(mode = "classification")$mode, "classification")
  expect_equal(tabular_auto_int(mode = "regression")$mode, "regression")

  expect_snapshot(error = TRUE, tabular_auto_int(mode = "quantile regression"))
})

test_that("tabular_auto_int() captures arguments as quosures", {
  spec <- tabular_auto_int(
    epochs = 50L,
    num_embedding = 8L,
    num_attn_heads = 4L,
    penalty = 0.01
  )

  expect_equal(rlang::quo_get_expr(spec$args$epochs), 50L)
  expect_equal(rlang::quo_get_expr(spec$args$num_embedding), 8L)
  expect_equal(rlang::quo_get_expr(spec$args$num_attn_heads), 4L)
  expect_equal(rlang::quo_get_expr(spec$args$penalty), 0.01)
})

test_that("update.tabular_auto_int() updates arguments", {
  spec <- tabular_auto_int(epochs = 50L, penalty = 0.01)

  updated <- update(spec, epochs = 100L)
  expect_equal(rlang::quo_get_expr(updated$args$epochs), 100L)
  expect_equal(rlang::quo_get_expr(updated$args$penalty), 0.01)

  fresh <- update(spec, epochs = 100L, fresh = TRUE)
  expect_equal(rlang::quo_get_expr(fresh$args$epochs), 100L)
  expect_null(rlang::quo_get_expr(fresh$args$penalty))
})

test_that("check_args.tabular_auto_int() validates input values", {
  expect_no_error(
    tabular_auto_int(mode = "regression", penalty = 0.1) |> check_args()
  )
  expect_no_error(
    tabular_auto_int(mode = "regression", dropout = 0.5) |> check_args()
  )
  expect_no_error(tabular_auto_int(mode = "regression") |> check_args())

  expect_snapshot(
    error = TRUE,
    tabular_auto_int(mode = "regression", penalty = -1) |> check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_auto_int(mode = "regression", mixture = 2) |> check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_auto_int(mode = "regression", dropout_attn = 1.5) |> check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_auto_int(mode = "regression", dropout_embedding = -0.1) |>
      check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_auto_int(mode = "regression", epochs = 2.5) |> check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_auto_int(mode = "regression", num_attn_heads = 0L) |> check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_auto_int(mode = "regression", stop_iter = 0L) |> check_args()
  )
})

test_that("check_args.tabular_auto_int() rejects both penalty and dropout", {
  spec <- tabular_auto_int(mode = "regression", penalty = 0.1, dropout = 0.2)
  expect_snapshot(error = TRUE, check_args(spec))
})
