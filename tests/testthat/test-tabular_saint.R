test_that("tabular_saint() creates a model spec with correct defaults", {
  spec <- tabular_saint()

  expect_s3_class(spec, "tabular_saint")
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "unknown")
  expect_equal(spec$engine, "brulee")
})

test_that("tabular_saint() accepts its modes", {
  expect_equal(tabular_saint(mode = "classification")$mode, "classification")
  expect_equal(tabular_saint(mode = "regression")$mode, "regression")

  expect_snapshot(error = TRUE, tabular_saint(mode = "quantile regression"))
})

test_that("tabular_saint() captures arguments as quosures", {
  spec <- tabular_saint(
    epochs = 50L,
    num_embedding = 8L,
    attention_type = "column",
    target_token = FALSE
  )

  expect_equal(rlang::quo_get_expr(spec$args$epochs), 50L)
  expect_equal(rlang::quo_get_expr(spec$args$num_embedding), 8L)
  expect_equal(rlang::quo_get_expr(spec$args$attention_type), "column")
  expect_false(rlang::quo_get_expr(spec$args$target_token))
})

test_that("update.tabular_saint() updates arguments", {
  spec <- tabular_saint(epochs = 50L, num_attn_blocks = 3L)

  updated <- update(spec, epochs = 100L)
  expect_equal(rlang::quo_get_expr(updated$args$epochs), 100L)
  expect_equal(rlang::quo_get_expr(updated$args$num_attn_blocks), 3L)

  fresh <- update(spec, epochs = 100L, fresh = TRUE)
  expect_null(rlang::quo_get_expr(fresh$args$num_attn_blocks))
})

test_that("check_args.tabular_saint() validates input values", {
  expect_no_error(
    tabular_saint(mode = "regression", attention_type = "both") |> check_args()
  )
  expect_no_error(
    tabular_saint(mode = "regression", penalty = 0.1) |> check_args()
  )
  expect_no_error(tabular_saint(mode = "regression") |> check_args())

  expect_snapshot(
    error = TRUE,
    tabular_saint(mode = "regression", attention_type = "diagonal") |>
      check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_saint(mode = "regression", mixture = -0.5) |> check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_saint(mode = "regression", dropout_attn = 1.5) |> check_args()
  )
  expect_snapshot(
    error = TRUE,
    tabular_saint(mode = "regression", num_attn_heads = 0L) |> check_args()
  )
})
