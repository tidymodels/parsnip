skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

is_keras3_ok <- function() {
  tryCatch(
    {
      keras3::set_random_seed(1L)
      TRUE
    },
    error = function(e) FALSE
  )
}

# ------------------------------------------------------------------------------

basic_mod <-
  linear_reg() |>
  set_engine("keras3", epochs = 50, verbose = 0)

ridge_mod <-
  linear_reg(penalty = 0.1) |>
  set_engine("keras3", epochs = 50, verbose = 0)

ctrl <- control_parsnip(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('model fitting', {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!is_keras3_ok())

  keras3::set_random_seed(257L)

  expect_no_condition(
    fit1 <-
      fit_xy(
        basic_mod,
        control = ctrl,
        x = hpc[, 2:4],
        y = hpc$compounds
      )
  )

  keras3::set_random_seed(257L)

  expect_no_condition(
    fit2 <-
      fit_xy(
        basic_mod,
        control = ctrl,
        x = hpc[, 2:4],
        y = hpc$compounds
      )
  )
  expect_equal(
    unlist(keras3::get_weights(extract_fit_engine(fit1))),
    unlist(keras3::get_weights(extract_fit_engine(fit2))),
    tolerance = 0.1
  )

  expect_no_condition(
    fit(
      basic_mod,
      compounds ~ .,
      data = hpc[, -5],
      control = ctrl
    )
  )

  expect_no_condition(
    fit1 <-
      fit_xy(
        ridge_mod,
        control = ctrl,
        x = hpc[, 2:4],
        y = hpc$compounds
      )
  )

  expect_no_condition(
    fit(
      ridge_mod,
      compounds ~ .,
      data = hpc[, -5],
      control = ctrl
    )
  )
})


test_that('regression prediction', {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!is_keras3_ok())

  keras3::set_random_seed(257L)

  lm_fit <-
    fit_xy(
      basic_mod,
      control = ctrl,
      x = hpc[, 2:4],
      y = hpc$compounds
    )

  keras3_pred <-
    predict(extract_fit_engine(lm_fit), as.matrix(hpc[1:3, 2:4]))
  colnames(keras3_pred) <- ".pred"
  keras3_pred <- tibble::as_tibble(keras3_pred)

  parsnip_pred <- predict(lm_fit, hpc[1:3, 2:4])
  expect_equal(as.data.frame(keras3_pred), as.data.frame(parsnip_pred))

  keras3::set_random_seed(257L)

  rr_fit <-
    fit_xy(
      ridge_mod,
      control = ctrl,
      x = hpc[, 2:4],
      y = hpc$compounds
    )

  keras3_pred <- predict(extract_fit_engine(rr_fit), as.matrix(hpc[1:3, 2:4]))
  colnames(keras3_pred) <- ".pred"
  keras3_pred <- tibble::as_tibble(keras3_pred)

  parsnip_pred <- predict(rr_fit, hpc[1:3, 2:4])
  expect_equal(as.data.frame(keras3_pred), as.data.frame(parsnip_pred))
})
