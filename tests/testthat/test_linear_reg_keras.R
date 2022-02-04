library(testthat)
library(parsnip)
library(rlang)
library(tibble)

# ------------------------------------------------------------------------------

context("keras linear regression")
source(test_path("helpers.R"))
source(test_path("helper-objects.R"))
hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

basic_mod <-
  linear_reg() %>%
  set_engine("keras", epochs = 50, verbose = 0)

ridge_mod <-
  linear_reg(penalty = 0.1) %>%
  set_engine("keras", epochs = 50, verbose = 0)

ctrl <- control_parsnip(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('model fitting', {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(is.null(tensorflow::tf_version()))

  set.seed(257)
  if (tensorflow::tf_version() >= package_version("2.0")) {
    tensorflow::tf$random$set_seed(257)
  } else {
    tensorflow::tf$random$set_random_seed(257)
  }
  expect_error(
    fit1 <-
      fit_xy(
        basic_mod,
        control = ctrl,
        x = hpc[,2:4],
        y = hpc$compounds
      ),
    regexp = NA
  )

  set.seed(257)
  if (tensorflow::tf_version() >= package_version("2.0")) {
    tensorflow::tf$random$set_seed(257)
  } else {
    tensorflow::tf$random$set_random_seed(257)
  }
  expect_error(
    fit2 <-
      fit_xy(
        basic_mod,
        control = ctrl,
        x = hpc[,2:4],
        y = hpc$compounds
      ),
    regexp = NA
  )
  expect_equal(
    unlist(keras::get_weights(fit1$fit)),
    unlist(keras::get_weights(fit2$fit)),
    tolerance = .1
  )

  expect_error(
    fit(
      basic_mod,
      compounds ~ .,
      data = hpc[, -5],
      control = ctrl
    ),
    regexp = NA
  )

  expect_error(
    fit1 <-
      fit_xy(
        ridge_mod,
        control = ctrl,
        x = hpc[,2:4],
        y = hpc$compounds
      ),
    regexp = NA
  )

  expect_error(
    fit(
      ridge_mod,
      compounds ~ .,
      data = hpc[, -5],
      control = ctrl
    ),
    regexp = NA
  )

})


test_that('regression prediction', {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(is.null(tensorflow::tf_version()))

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
    predict(lm_fit$fit, as.matrix(hpc[1:3,2:4]))
  colnames(keras_pred) <- ".pred"

  keras_pred <-
    keras_pred %>%
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

  keras_pred <- predict(rr_fit$fit, as.matrix(hpc[1:3,2:4]))
  colnames(keras_pred) <- ".pred"
  keras_pred <- as_tibble(keras_pred)

  parsnip_pred <- predict(rr_fit, hpc[1:3,2:4])
  expect_equal(as.data.frame(keras_pred), as.data.frame(parsnip_pred))

})
