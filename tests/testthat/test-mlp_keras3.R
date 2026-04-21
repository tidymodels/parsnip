skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

num_pred <- names(hpc)[1:4]

hpc_keras3 <-
  mlp(mode = "classification", hidden_units = 2, epochs = 10) |>
  set_engine("keras3", verbose = 0)

nn_dat <- read.csv("nnet_test.txt")

is_keras3_ok <- function() {
  tryCatch({
    keras3::set_random_seed(1L)
    TRUE
  }, error = function(e) FALSE)
}

# ------------------------------------------------------------------------------

test_that('keras3 execution, classification', {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!is_keras3_ok())

  expect_no_condition(
    res <- parsnip::fit(
      hpc_keras3,
      class ~ compounds + input_fields,
      data = hpc,
      control = ctrl
    )
  )

  expect_false(has_multi_predict(res))
  expect_equal(multi_predict_args(res), NA_character_)

  keras3::clear_session()

  expect_no_condition(
    res <- parsnip::fit_xy(
      hpc_keras3,
      x = hpc[, num_pred],
      y = hpc$class,
      control = ctrl
    )
  )

  keras3::clear_session()

  expect_snapshot(
    error = TRUE,
    res <- parsnip::fit(
      hpc_keras3,
      class ~ novar,
      data = hpc,
      control = ctrl
    )
  )
})


test_that('keras3 classification prediction', {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!is_keras3_ok())

  xy_fit <- parsnip::fit_xy(
    hpc_keras3,
    x = hpc[, num_pred],
    y = hpc$class,
    control = ctrl
  )

  xy_pred_raw <- predict(
    extract_fit_engine(xy_fit),
    x = as.matrix(hpc[1:8, num_pred])
  )
  xy_pred <- factor(
    levels(hpc$class)[as.integer(keras3::op_argmax(xy_pred_raw, axis = 2L)) + 1L],
    levels = levels(hpc$class)
  )
  expect_equal(
    xy_pred,
    predict(xy_fit, new_data = hpc[1:8, num_pred], type = "class")[[".pred_class"]]
  )

  keras3::clear_session()

  form_fit <- parsnip::fit(
    hpc_keras3,
    class ~ .,
    data = hpc,
    control = ctrl
  )

  form_pred_raw <- predict(
    extract_fit_engine(form_fit),
    x = as.matrix(hpc[1:8, num_pred])
  )
  form_pred <- factor(
    levels(hpc$class)[as.integer(keras3::op_argmax(form_pred_raw, axis = 2L)) + 1L],
    levels = levels(hpc$class)
  )
  expect_equal(
    form_pred,
    predict(form_fit, new_data = hpc[1:8, num_pred], type = "class")[[".pred_class"]]
  )

  keras3::clear_session()
})


test_that('keras3 classification probabilities', {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!is_keras3_ok())

  xy_fit <- parsnip::fit_xy(
    hpc_keras3,
    x = hpc[, num_pred],
    y = hpc$class,
    control = ctrl
  )

  xy_pred <- predict(
    extract_fit_engine(xy_fit),
    x = as.matrix(hpc[1:8, num_pred])
  )
  colnames(xy_pred) <- paste0(".pred_", levels(hpc$class))
  xy_pred <- tibble::as_tibble(xy_pred)
  expect_equal(
    xy_pred,
    predict(xy_fit, new_data = hpc[1:8, num_pred], type = "prob")
  )

  keras3::clear_session()

  form_fit <- parsnip::fit(
    hpc_keras3,
    class ~ .,
    data = hpc,
    control = ctrl
  )

  form_pred <- predict(
    extract_fit_engine(form_fit),
    x = as.matrix(hpc[1:8, num_pred])
  )
  colnames(form_pred) <- paste0(".pred_", levels(hpc$class))
  form_pred <- tibble::as_tibble(form_pred)
  expect_equal(
    form_pred,
    predict(form_fit, new_data = hpc[1:8, num_pred], type = "prob")
  )

  keras3::clear_session()
})

# ------------------------------------------------------------------------------

mtcars <- as.data.frame(scale(mtcars))

num_pred <- names(mtcars)[3:6]

car_basic <- mlp(mode = "regression", epochs = 10) |>
  set_engine("keras3", verbose = 0)

# ------------------------------------------------------------------------------

test_that('keras3 execution, regression', {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!is_keras3_ok())

  expect_no_condition(
    res <- parsnip::fit(
      car_basic,
      mpg ~ .,
      data = mtcars,
      control = ctrl
    )
  )

  keras3::clear_session()

  expect_no_condition(
    res <- parsnip::fit_xy(
      car_basic,
      x = mtcars[, num_pred],
      y = mtcars$mpg,
      control = ctrl
    )
  )
})

test_that('keras3 regression prediction', {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!is_keras3_ok())

  xy_fit <- parsnip::fit_xy(
    mlp(mode = "regression", hidden_units = 2, epochs = 500, penalty = 0.1) |>
      set_engine("keras3", verbose = 0),
    x = mtcars[, c("cyl", "disp")],
    y = mtcars$mpg,
    control = ctrl
  )

  xy_pred <- predict(
    extract_fit_engine(xy_fit),
    x = as.matrix(mtcars[1:8, c("cyl", "disp")])
  )[, 1]
  expect_equal(
    xy_pred,
    predict(xy_fit, new_data = mtcars[1:8, c("cyl", "disp")])[[".pred"]]
  )

  keras3::clear_session()

  form_fit <- parsnip::fit(
    car_basic,
    mpg ~ .,
    data = mtcars[, c("cyl", "disp", "mpg")],
    control = ctrl
  )

  form_pred <- predict(
    extract_fit_engine(form_fit),
    x = as.matrix(mtcars[1:8, c("cyl", "disp")])
  )[, 1]
  expect_equal(
    form_pred,
    predict(form_fit, new_data = mtcars[1:8, c("cyl", "disp")])[[".pred"]]
  )

  keras3::clear_session()
})

# ------------------------------------------------------------------------------

test_that('multivariate keras3 formula', {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!is_keras3_ok())

  nnet_form <-
    mlp(mode = "regression", hidden_units = 3, penalty = 0.01) |>
    set_engine("keras3", verbose = 0) |>
    parsnip::fit(
      cbind(V1, V2, V3) ~ .,
      data = nn_dat[-(1:5), ]
    )
  expect_equal(
    length(unlist(keras3::get_weights(extract_fit_engine(nnet_form)))),
    24
  )

  nnet_form_pred <- predict(nnet_form, new_data = nn_dat[1:5, -(1:3)])
  expect_equal(names(nnet_form_pred), paste0(".pred_", c("V1", "V2", "V3")))

  keras3::clear_session()

  nnet_xy <-
    mlp(mode = "regression", hidden_units = 3, penalty = 0.01) |>
    set_engine("keras3", verbose = 0) |>
    parsnip::fit_xy(
      x = nn_dat[-(1:5), -(1:3)],
      y = nn_dat[-(1:5), 1:3]
    )
  expect_equal(
    length(unlist(keras3::get_weights(extract_fit_engine(nnet_xy)))),
    24
  )
  nnet_form_xy <- predict(nnet_xy, new_data = nn_dat[1:5, -(1:3)])
  expect_equal(names(nnet_form_xy), paste0(".pred_", c("V1", "V2", "V3")))

  keras3::clear_session()
})

# ------------------------------------------------------------------------------

test_that('all keras3 activation functions', {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("dials", minimum_version = "1.3.0.9000")
  skip_if(!is_keras3_ok())

  act <- keras3_activations()

  test_act <- function(fn) {
    set.seed(1)
    try(
      mlp(
        mode = "classification",
        hidden_units = 2,
        penalty = 0.01,
        epochs = 2,
        activation = !!fn
      ) |>
        set_engine("keras3", verbose = 0) |>
        parsnip::fit(Class ~ A + B, data = modeldata::two_class_dat),
      silent = TRUE
    )
  }
  test_act_sshhh <- purrr::quietly(test_act)

  for (i in act) {
    keras3::clear_session()
    act_res <- test_act_sshhh(i)
    expect_s3_class(act_res$result, "model_fit")
    keras3::clear_session()
  }

  expect_snapshot(
    error = TRUE,
    mlp(
      mode = "classification",
      hidden_units = 2,
      penalty = 0.01,
      epochs = 2,
      activation = "invalid"
    ) |>
      set_engine("keras3", verbose = 0) |>
      parsnip::fit(Class ~ A + B, data = modeldata::two_class_dat)
  )
})
