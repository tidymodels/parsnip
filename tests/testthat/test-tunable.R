test_that('brulee has mixture object', {
  skip_if_not_installed("brulee")
  # for issue 1236
  mlp_spec <-
    mlp(
      hidden_units = tune(),
      activation =  tune(),
      penalty = tune(),
      learn_rate = tune(),
      epoch = 2000
    ) |>
    set_mode("regression") |>
    set_engine("brulee",
               stop_iter = tune(),
               mixture = tune(),
               rate_schedule = tune())

  brulee_res <- tunable(mlp_spec)

  expect_true(
    length(brulee_res$call_info[brulee_res$name == "mixture"]) > 0
  )
})
