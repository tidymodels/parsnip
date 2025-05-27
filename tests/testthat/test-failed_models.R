skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]


# ------------------------------------------------------------------------------

hpc_bad <-
  hpc |>
  dplyr::mutate(big_num = Inf)

lending_club <-
  lending_club |>
  dplyr::slice(1:200) |>
  dplyr::mutate(big_num = Inf)

lvl <- levels(lending_club$Class)

# ------------------------------------------------------------------------------

ctrl <- control_parsnip(catch = TRUE)

# ------------------------------------------------------------------------------

test_that('numeric model', {
  lm_mod <-
    linear_reg() |>
    set_engine("lm") |>
    fit(compounds ~ ., data = hpc_bad, control = ctrl)

  expect_snapshot(num_res <- predict(lm_mod, hpc_bad[1:11, -1]))
  expect_equal(num_res, NULL)

  expect_snapshot(ci_res <- predict(lm_mod, hpc_bad[1:11, -1], type = "conf_int"))
  expect_equal(ci_res, NULL)

  expect_snapshot(pi_res <- predict(lm_mod, hpc_bad[1:11, -1], type = "pred_int"))
  expect_equal(pi_res, NULL)

})

# ------------------------------------------------------------------------------

test_that('classification model', {
  log_reg <-
    logistic_reg() |>
    set_engine("glm") |>
    fit(Class ~ log(funded_amnt) + int_rate + big_num, data = lending_club, control = ctrl)

  expect_snapshot(
    cls_res <-
      predict(log_reg, lending_club |>  dplyr::slice(1:7) |> dplyr::select(-Class))
  )
  expect_equal(cls_res, NULL)

  expect_snapshot(
    prb_res <-
      predict(log_reg, lending_club |>  dplyr::slice(1:7) |> dplyr::select(-Class), type = "prob")
  )
  expect_equal(prb_res, NULL)

  expect_snapshot(
    ci_res <-
      predict(log_reg, lending_club |>  dplyr::slice(1:7) |> dplyr::select(-Class), type = "conf_int")
  )
  expect_equal(ci_res, NULL)
})

