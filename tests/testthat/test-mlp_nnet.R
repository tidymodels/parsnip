skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

num_pred <- names(hpc)[1:4]

hpc_nnet <-
  mlp(mode = "classification", hidden_units = 5) |>
  set_engine("nnet")

# ------------------------------------------------------------------------------

test_that('nnet execution, classification', {

  skip_if_not_installed("nnet")

  expect_no_condition(
    res <- parsnip::fit(
      hpc_nnet,
      class ~ compounds + input_fields,
      data = hpc,
      control = ctrl
    )
  )

  expect_no_condition(
    res <- parsnip::fit_xy(
      hpc_nnet,
      x = hpc[, num_pred],
      y = hpc$class,
      control = ctrl
    )
  )

  expect_snapshot(
    error = TRUE,
    res <- parsnip::fit(
      hpc_nnet,
      class ~ novar,
      data = hpc,
      control = ctrl
    )
  )
})


test_that('nnet classification prediction', {

  skip_if_not_installed("nnet")

  xy_fit <- fit_xy(
    hpc_nnet,
    x = hpc[, num_pred],
    y = hpc$class,
    control = ctrl
  )

  xy_pred <- predict(extract_fit_engine(xy_fit), newdata = hpc[1:8, num_pred], type = "class")
  xy_pred <- factor(xy_pred, levels = levels(hpc$class))
  expect_equal(xy_pred, predict(xy_fit, new_data = hpc[1:8, num_pred], type = "class")$.pred_class)

  form_fit <- fit(
    hpc_nnet,
    class ~ .,
    data = hpc,
    control = ctrl
  )

  form_pred <- predict(extract_fit_engine(form_fit), newdata = hpc[1:8, num_pred], type = "class")
  form_pred <- factor(form_pred, levels = levels(hpc$class))
  expect_equal(form_pred, predict(form_fit, new_data = hpc[1:8, num_pred])$.pred_class)
})


# ------------------------------------------------------------------------------

num_pred <- names(mtcars)[3:6]

car_basic <-
  mlp(mode = "regression") |>
  set_engine("nnet")

bad_nnet_reg <-
  mlp(mode = "regression") |>
  set_engine("nnet", min.node.size = -10)
bad_rf_reg <-
  mlp(mode = "regression") |>
  set_engine("nnet", sampsize = -10)

# ------------------------------------------------------------------------------


test_that('nnet execution, regression', {

  skip_if_not_installed("nnet")

  expect_no_condition(
    res <- parsnip::fit(
      car_basic,
      mpg ~ .,
      data = mtcars,
      control = ctrl
    )
  )

  expect_no_condition(
    res <- parsnip::fit_xy(
      car_basic,
      x = mtcars[, num_pred],
      y = mtcars$mpg,
      control = ctrl
    )
  )
})



test_that('nnet regression prediction', {

  skip_if_not_installed("nnet")

  xy_fit <- fit_xy(
    car_basic,
    x = mtcars[, -1],
    y = mtcars$mpg,
    control = ctrl
  )

  xy_pred <- predict(extract_fit_engine(xy_fit), newdata = mtcars[1:8, -1])[,1]
  xy_pred <- unname(xy_pred)
  expect_equal(xy_pred, predict(xy_fit, new_data = mtcars[1:8, -1])$.pred)

  form_fit <- fit(
    car_basic,
    mpg ~ .,
    data = mtcars,
    control = ctrl
  )

  form_pred <- predict(extract_fit_engine(form_fit), newdata = mtcars[1:8, -1])[,1]
  form_pred <- unname(form_pred)
  expect_equal(form_pred, predict(form_fit, new_data = mtcars[1:8, -1])$.pred)
})

# ------------------------------------------------------------------------------

nn_dat <- read.csv("nnet_test.txt")

test_that('multivariate nnet formula', {

  skip_if_not_installed("nnet")

  nnet_form <-
    mlp(
      mode = "regression",
      hidden_units = 3,
      penalty = 0.01
    )  |>
    set_engine("nnet") |>
    parsnip::fit(
      cbind(V1, V2, V3) ~ .,
      data = nn_dat[-(1:5),]
    )

  expect_false(has_multi_predict(nnet_form))
  expect_equal(multi_predict_args(nnet_form), NA_character_)

  expect_equal(length(extract_fit_engine(nnet_form)$wts), 24)
  nnet_form_pred <- predict(nnet_form, new_data = nn_dat[1:5, -(1:3)])
  expect_equal(names(nnet_form_pred), paste0(".pred_", c("V1", "V2", "V3")))

  nnet_xy <-
    mlp(
      mode = "regression",
      hidden_units = 3,
      penalty = 0.01
    ) |>
    set_engine("nnet") |>
    parsnip::fit_xy(
      x = nn_dat[-(1:5), -(1:3)],
      y = nn_dat[-(1:5),   1:3 ]
    )
  expect_equal(length(extract_fit_engine(nnet_xy)$wts), 24)
  nnet_form_xy <- predict(nnet_xy, new_data = nn_dat[1:5, -(1:3)])
  expect_equal(names(nnet_form_xy), paste0(".pred_", c("V1", "V2", "V3")))
})



