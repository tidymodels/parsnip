skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------
test_that('updating', {
  expect_snapshot(
    multinom_reg(mixture = 0) |>
      set_engine("glmnet", nlambda = 10) |>
      update(mixture = tune(), nlambda = tune())
  )
})

test_that('bad input', {
  expect_snapshot(error = TRUE, multinom_reg(mode = "regression"))
  expect_snapshot(error = TRUE, translate(multinom_reg(penalty = 0.1) |> set_engine("wat?")))
  expect_snapshot(error = TRUE, multinom_reg(penalty = 0.1) |> set_engine())
  expect_warning(
    translate(
      multinom_reg(penalty = 0.1) |> set_engine("glmnet", x = hpc[,1:3], y = hpc$class)
    ),
    class = "parsnip_protected_arg_warning"
  )
})

test_that('check_args() works', {
  skip_if_not_installed("keras")

  expect_snapshot(
    error = TRUE,
    {
      spec <- multinom_reg(mixture = -1) |>
        set_engine("keras") |>
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      spec <- multinom_reg(penalty = -1) |>
        set_engine("keras") |>
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
})

# ------------------------------------------------------------------------------

test_that("tunables", {

  expect_snapshot(
    multinom_reg() |>
      tunable()
  )

  expect_snapshot(
    multinom_reg() |>
      set_engine("brulee") |>
      tunable()
  )

  expect_snapshot(
    multinom_reg() |>
      set_engine("nnet") |>
      tunable()
  )

  expect_snapshot(
    multinom_reg() |>
      set_engine("glmnet") |>
      tunable()
  )

  expect_snapshot(
    multinom_reg() |>
      set_engine("keras") |>
      tunable()
  )

})
