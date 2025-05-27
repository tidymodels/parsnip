skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

basic_mod <-
  linear_reg() |>
  set_engine("keras", epochs = 50, verbose = 0)

ridge_mod <-
  linear_reg(penalty = 0.1) |>
  set_engine("keras", epochs = 50, verbose = 0)

ctrl <- control_parsnip(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('model fitting', {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(!is_tf_ok())

  set_tf_seed(257)

  expect_no_condition(
    fit1 <-
      fit_xy(
        basic_mod,
        control = ctrl,
        x = hpc[,2:4],
        y = hpc$compounds
      )
  )

  set_tf_seed(257)

  expect_no_condition(
    fit2 <-
      fit_xy(
        basic_mod,
        control = ctrl,
        x = hpc[,2:4],
        y = hpc$compounds
      )
  )
  expect_equal(
    unlist(keras::get_weights(extract_fit_engine(fit1))),
    unlist(keras::get_weights(extract_fit_engine(fit2))),
    tolerance = .1
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
        x = hpc[,2:4],
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
  skip_if_not_installed("keras")
  skip_if(!is_tf_ok())

  library(keras)

  set.seed(257)
  lm_fit <-
    fit_xy(
      basic_mod,
      control = ctrl,
      x = hpc[,2:4],
      y = hpc$compounds
    )

  keras_pred <-
    predict(extract_fit_engine(lm_fit), as.matrix(hpc[1:3,2:4]))
  colnames(keras_pred) <- ".pred"

  keras_pred <-
    keras_pred |>
    as_tibble()
  parsnip_pred <- predict(lm_fit, hpc[1:3,2:4])
  expect_equal(as.data.frame(keras_pred), as.data.frame(parsnip_pred))

  set.seed(257)
  rr_fit <-
    fit_xy(
      ridge_mod,
      control = ctrl,
      x = hpc[,2:4],
      y = hpc$compounds
    )

  keras_pred <- predict(extract_fit_engine(rr_fit), as.matrix(hpc[1:3,2:4]))
  colnames(keras_pred) <- ".pred"
  keras_pred <- tibble::as_tibble(keras_pred)

  parsnip_pred <- predict(rr_fit, hpc[1:3,2:4])
  expect_equal(as.data.frame(keras_pred), as.data.frame(parsnip_pred))

})
