test_that('pipe arguments', {
  mod_1 <- rand_forest() |>
    set_args(mtry = 1)
  expect_equal(
    rlang::quo_get_expr(mod_1$args$mtry),
    1
  )
  expect_equal(
    rlang::quo_get_env(mod_1$args$mtry),
    rlang::empty_env()
  )

  mod_2 <- rand_forest(mtry = 2) |>
    set_args(mtry = 1)

  var_env <- rlang::current_env()

  expect_equal(
    rlang::quo_get_expr(mod_2$args$mtry),
    1
  )
  expect_equal(
    rlang::quo_get_env(mod_2$args$mtry),
    rlang::empty_env()
  )

  expect_snapshot(error = TRUE, rand_forest() |> set_args())

})


test_that('pipe engine', {
  mod_1 <- rand_forest() |>
    set_mode("regression")
  expect_equal(mod_1$mode, "regression")

  expect_snapshot(error = TRUE, rand_forest() |> set_mode())
  expect_snapshot(error = TRUE, rand_forest() |> set_mode(2))
  expect_snapshot(error = TRUE, rand_forest() |> set_mode("haberdashery"))
})

test_that("can't set a mode that isn't allowed by the model spec", {
  expect_snapshot(
    set_mode(linear_reg(), "classification"),
    error = TRUE
  )
})



test_that("unavailable modes for an engine and vice-versa", {
  expect_snapshot(
    decision_tree() |>
      set_mode("regression") |>
      set_engine("C5.0"),
    error = TRUE
  )

  expect_snapshot(
    decision_tree(mode = "regression", engine = "C5.0"),
    error = TRUE
  )

  expect_snapshot(
    decision_tree() |>
      set_engine("C5.0") |>
      set_mode("regression"),
    error = TRUE
  )

  expect_snapshot(
    decision_tree(engine = NULL) |>
      set_engine("C5.0") |>
      set_mode("regression"),
    error = TRUE
  )

  expect_snapshot(
    decision_tree(engine = NULL)|>
      set_mode("regression") |>
      set_engine("C5.0"),
    error = TRUE
  )

  expect_snapshot(
    proportional_hazards() |> set_mode("regression"),
    error = TRUE
  )

  expect_snapshot(
    linear_reg() |> set_mode(),
    error = TRUE
  )

  expect_snapshot(
    linear_reg(engine = "boop"),
    error = TRUE
  )

  expect_snapshot(
    linear_reg() |> set_engine(),
    error = TRUE
  )

  expect_snapshot(
    proportional_hazards() |> set_engine(),
    error = TRUE
  )
})

test_that("set_* functions error when input isn't model_spec", {
  expect_snapshot(error = TRUE,
                  set_mode(mtcars, "regression")
  )

  expect_snapshot(error = TRUE,
                  set_args(mtcars, blah = "blah")
  )

  expect_snapshot(error = TRUE,
                  bag_tree |> set_mode("classification")
  )

  expect_snapshot(error = TRUE,
                  bag_tree |> set_engine("rpart")
  )

  expect_snapshot(error = TRUE,
                  bag_tree |> set_args(boop = "bop")
  )

  # won't raise "info" part of error if not a parsnip-namespaced function
  # not a function
  expect_snapshot(error = TRUE,
                  1L |> set_args(mode = "classification")
  )

  # not from parsnip
  expect_snapshot(error = TRUE,
                  bag_tree |> set_mode("classification")
  )
})

