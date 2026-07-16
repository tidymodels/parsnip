test_that("tabular_icl() creates a model spec with correct defaults", {
  spec <- tabular_icl()

  expect_s3_class(spec, "tabular_icl")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "unknown")
  expect_equal(spec$engine, "brulee")
})

test_that("tabular_icl() accepts its modes", {
  expect_equal(tabular_icl(mode = "classification")$mode, "classification")
  expect_equal(tabular_icl(mode = "regression")$mode, "regression")

  expect_snapshot(error = TRUE, tabular_icl(mode = "censored regression"))
})

test_that("tabular_icl() captures arguments as quosures", {
  spec <- tabular_icl(num_estimators = 4L, softmax_temperature = 0.5)

  expect_equal(rlang::quo_get_expr(spec$args$num_estimators), 4L)
  expect_equal(rlang::quo_get_expr(spec$args$softmax_temperature), 0.5)
})

test_that("update.tabular_icl() updates arguments", {
  spec <- tabular_icl(num_estimators = 4L, softmax_temperature = 0.5)

  updated <- update(spec, num_estimators = 16L)
  expect_equal(rlang::quo_get_expr(updated$args$num_estimators), 16L)
  expect_equal(rlang::quo_get_expr(updated$args$softmax_temperature), 0.5)

  fresh <- update(spec, num_estimators = 16L, fresh = TRUE)
  expect_null(rlang::quo_get_expr(fresh$args$softmax_temperature))
})

test_that("check_args.tabular_icl() validates input values", {
  expect_no_error(
    tabular_icl(mode = "classification", num_estimators = 4L) |> check_args()
  )
  expect_no_error(tabular_icl(mode = "regression") |> check_args())

  expect_snapshot(
    error = TRUE,
    tabular_icl(mode = "classification", softmax_temperature = -1) |>
      check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_icl(mode = "classification", num_estimators = 5.5) |> check_args()
  )
})
