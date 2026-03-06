test_that('brulee has mixture object', {
  skip_if_not_installed("brulee")
  # for issue 1236
  mlp_spec <-
    mlp(
      hidden_units = tune(),
      activation = tune(),
      penalty = tune(),
      learn_rate = tune(),
      epoch = 2000
    ) |>
    set_mode("regression") |>
    set_engine(
      "brulee",
      stop_iter = tune(),
      mixture = tune(),
      rate_schedule = tune()
    )

  brulee_res <- tunable(mlp_spec)

  expect_true(
    length(brulee_res$call_info[brulee_res$name == "mixture"]) > 0
  )
})

test_that('apply_tunable_spec updates mechanism works', {
  # Test that the updates mechanism in apply_tunable_spec() works correctly
  base_result <- tibble::tibble(
    name = c("param1", "param2"),
    call_info = list(
      list(pkg = "dials", fun = "original_fun"),
      list(pkg = "dials", fun = "other_fun")
    ),
    source = "model_spec",
    component = "test_model",
    component_id = "main"
  )

  test_spec <- list(
    test_engine = list(
      updates = list(
        param1 = list(pkg = "dials", fun = "updated_fun", range = c(0, 1))
      )
    )
  )

  result <- parsnip:::apply_tunable_spec(base_result, "test_engine", test_spec)

  expect_equal(result$call_info[[1]]$fun, "updated_fun")
  expect_equal(result$call_info[[1]]$range, c(0, 1))
  expect_equal(result$call_info[[2]]$fun, "other_fun")
})
