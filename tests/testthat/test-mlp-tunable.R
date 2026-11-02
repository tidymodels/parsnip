test_that("tunable parameters for mlp + keras", {
  expect_snapshot(
    display_tunable_call_info(
      mlp() |> set_engine("keras")
    )
  )
})

test_that("tunable parameters for mlp + nnet", {
  expect_snapshot(
    display_tunable_call_info(
      mlp() |> set_engine("nnet")
    )
  )
})

test_that("tunable parameters for mlp + brulee", {
  expect_snapshot(
    display_tunable_call_info(
      mlp() |> set_engine("brulee")
    )
  )
})

test_that("tunable parameters for mlp + brulee_two_layer", {
  expect_snapshot(
    display_tunable_call_info(
      mlp() |> set_engine("brulee_two_layer")
    )
  )
})

test_that("tunable parameters for mlp + qrnn", {
  expect_snapshot(
    display_tunable_call_info(
      mlp() |>
        set_engine("qrnn") |>
        set_mode("quantile regression", quantile_levels = (1:10) / 10)
    )
  )
})

test_that("brulee scheduler and validation engine args are tunable", {
  res <- tunable(mlp() |> set_engine("brulee"))
  eng_args <- c(
    validation = "validation_set_prop",
    initial = "rate_initial",
    largest = "rate_largest",
    reduction = "rate_reduction",
    steps = "rate_steps",
    step_size = "rate_step_size",
    decay = "rate_decay"
  )
  res <- res[match(names(eng_args), res$name), ]
  expect_identical(res$name, names(eng_args))
  expect_identical(purrr::map_chr(res$call_info, "fun"), unname(eng_args))
  expect_all_equal(res$component_id, "engine")
})
