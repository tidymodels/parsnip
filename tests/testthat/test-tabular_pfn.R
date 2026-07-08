test_that("tabular_pfn() creates a model spec with correct defaults", {
  spec <- tabular_pfn()

  expect_s3_class(spec, "tabular_pfn")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "unknown")
  expect_equal(spec$engine, "tabpfn")
})

test_that("tabular_pfn() accepts its modes", {
  expect_equal(tabular_pfn(mode = "classification")$mode, "classification")
  expect_equal(tabular_pfn(mode = "regression")$mode, "regression")

  expect_snapshot(error = TRUE, tabular_pfn(mode = "quantile regression"))
})

test_that("tabular_pfn() captures arguments as quosures", {
  spec <- tabular_pfn(
    num_estimators = 4L,
    softmax_temperature = 0.5,
    balance_probabilities = TRUE,
    average_before_softmax = FALSE
  )

  expect_equal(rlang::quo_get_expr(spec$args$num_estimators), 4L)
  expect_equal(rlang::quo_get_expr(spec$args$softmax_temperature), 0.5)
  expect_true(rlang::quo_get_expr(spec$args$balance_probabilities))
  expect_false(rlang::quo_get_expr(spec$args$average_before_softmax))
})

test_that("update.tabular_pfn() updates arguments", {
  spec <- tabular_pfn(num_estimators = 4L, balance_probabilities = TRUE)

  updated <- update(spec, num_estimators = 16L)
  expect_equal(rlang::quo_get_expr(updated$args$num_estimators), 16L)
  expect_true(rlang::quo_get_expr(updated$args$balance_probabilities))

  fresh <- update(spec, num_estimators = 16L, fresh = TRUE)
  expect_null(rlang::quo_get_expr(fresh$args$balance_probabilities))
})

test_that("check_args.tabular_pfn() validates input values", {
  expect_no_error(
    tabular_pfn(mode = "classification", num_estimators = 4L) |> check_args()
  )
  expect_no_error(tabular_pfn(mode = "regression") |> check_args())

  expect_snapshot(
    error = TRUE,
    tabular_pfn(mode = "classification", softmax_temperature = -1) |>
      check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_pfn(mode = "classification", num_estimators = 5.5) |> check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_pfn(mode = "classification", balance_probabilities = "yes") |>
      check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_pfn(mode = "classification", average_before_softmax = 1) |>
      check_args()
  )
})
