test_that("tabular_rln() creates a model spec with correct defaults", {
  spec <- tabular_rln()

  expect_s3_class(spec, "tabular_rln")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "brulee")
})

test_that("tabular_rln() is regression only", {
  expect_snapshot(error = TRUE, tabular_rln(mode = "classification"))
})

test_that("tabular_rln() captures arguments as quosures", {
  spec <- tabular_rln(
    hidden_units = 8L,
    penalty_type = "L2",
    penalty_average = 1e-6,
    step_rate = 1e4
  )

  expect_equal(rlang::quo_get_expr(spec$args$hidden_units), 8L)
  expect_equal(rlang::quo_get_expr(spec$args$penalty_type), "L2")
  expect_equal(rlang::quo_get_expr(spec$args$penalty_average), 1e-6)
  expect_equal(rlang::quo_get_expr(spec$args$step_rate), 1e4)
})

test_that("update.tabular_rln() updates arguments", {
  spec <- tabular_rln(hidden_units = 8L, epochs = 50L)

  updated <- update(spec, hidden_units = 32L)
  expect_equal(rlang::quo_get_expr(updated$args$hidden_units), 32L)
  expect_equal(rlang::quo_get_expr(updated$args$epochs), 50L)

  fresh <- update(spec, hidden_units = 32L, fresh = TRUE)
  expect_null(rlang::quo_get_expr(fresh$args$epochs))
})

test_that("check_args.tabular_rln() validates input values", {
  expect_no_error(tabular_rln(penalty_type = "L1") |> check_args())
  expect_no_error(tabular_rln(penalty_type = "L2") |> check_args())
  expect_no_error(tabular_rln() |> check_args())

  expect_snapshot(
    error = TRUE,
    tabular_rln(penalty_type = "L3") |> check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_rln(penalty_average = -1) |> check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_rln(step_rate = -5) |> check_args()
  )
})
