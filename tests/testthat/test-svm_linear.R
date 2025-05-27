skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('updating', {
  expect_snapshot(
    svm_linear(mode = "regression", cost = 2) |>
      set_engine("kernlab", cross = 10) |>
      update(cross = tune(), cost = tune())
  )
})

test_that('bad input', {
  expect_snapshot(error = TRUE, translate(svm_linear(mode = "regression") |> set_engine( NULL)))
  expect_snapshot(error = TRUE, svm_linear(mode = "reallyunknown"))
  expect_snapshot(error = TRUE, translate(svm_linear(mode = "regression") |> set_engine("LiblineaR", type = 3)))
  expect_snapshot(error = TRUE, translate(svm_linear(mode = "classification") |> set_engine("LiblineaR", type = 11)))
})

# ------------------------------------------------------------------------------

reg_mod <-
  svm_linear(mode = "regression", cost = 1/4) |>
  set_engine("LiblineaR") |>
  set_mode("regression")

cls_mod <-
  svm_linear(mode = "classification", cost = 1/8) |>
  set_engine("LiblineaR") |>
  set_mode("classification")

ctrl <- control_parsnip(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('linear svm regression: LiblineaR', {

  skip_if_not_installed("LiblineaR")

  expect_no_condition(
    res <- fit_xy(
      reg_mod,
      control = ctrl,
      x = hpc[,2:4],
      y = hpc$input_fields
    )
  )
  expect_false(has_multi_predict(res))
  expect_equal(multi_predict_args(res), NA_character_)

  expect_no_condition(tidy_res <- tidy(res))
  expect_s3_class(tidy_res, "tbl_df")
  expect_equal(colnames(tidy_res), c("term", "estimate"))

  expect_no_condition(
    fit(
      reg_mod,
      input_fields ~ .,
      data = hpc[, -5],
      control = ctrl
    )
  )

})


test_that('linear svm regression prediction: LiblineaR', {

  skip_if_not_installed("LiblineaR")

  hpc_no_m <- hpc[-c(84, 85, 86, 87, 88, 109, 128),] |>
    droplevels()

  ind <- c(2, 1, 143)

  reg_form <-
    fit(
      reg_mod,
      input_fields ~ .,
      data = hpc[, -5],
      control = ctrl
    )

  liblinear_pred <-
    structure(
      list(.pred = c(85.13979, 576.16232, 1886.10132)),
      row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

  parsnip_pred <- predict(reg_form, hpc[ind, -c(2, 5)])
  expect_equal(as.data.frame(liblinear_pred),
               as.data.frame(parsnip_pred),
               tolerance = .0001)


  reg_xy_form <-
    fit_xy(
      reg_mod,
      x = hpc[, c(1, 3, 4)],
      y = hpc$input_fields,
      control = ctrl
    )
  expect_equal(extract_fit_engine(reg_form)$W, extract_fit_engine(reg_xy_form)$W)

  parsnip_xy_pred <- predict(reg_xy_form, hpc[ind, -c(2, 5)])
  expect_equal(as.data.frame(liblinear_pred),
               as.data.frame(parsnip_xy_pred),
               tolerance = .0001)
})

# ------------------------------------------------------------------------------

test_that('linear svm classification: LiblineaR', {

  skip_if_not_installed("LiblineaR")

  hpc_no_m <- hpc[-c(84, 85, 86, 87, 88, 109, 128),] |>
    droplevels()

  ind <- c(2, 1, 143)

  expect_no_condition(
    fit_xy(
      cls_mod,
      control = ctrl,
      x = hpc_no_m[, -5],
      y = hpc_no_m$class
    )
  )

  expect_no_condition(
    fit(
      cls_mod,
      class ~ .,
      data = hpc_no_m,
      control = ctrl
    )
  )

})


test_that('linear svm classification prediction: LiblineaR', {

  skip_if_not_installed("LiblineaR")

  hpc_no_m <- hpc[-c(84, 85, 86, 87, 88, 109, 128),] |>
    droplevels()

  ind <- c(4, 55, 143)

  set.seed(34562)
  cls_form <-
    fit(
      cls_mod,
      class ~ .,
      data = hpc_no_m,
      control = ctrl
    )

  liblinear_class <-
    structure(list(
      .pred_class = structure(
        c(1L, 1L, 2L),
        .Label = c("VF", "F", "L"), class = "factor")),
      row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

  parsnip_class <- predict(cls_form, hpc_no_m[ind, -5])
  expect_equal(liblinear_class, parsnip_class)

  set.seed(34562)
  cls_xy_form <-
    fit_xy(
      cls_mod,
      x = hpc_no_m[, 1:4],
      y = hpc_no_m$class,
      control = ctrl
    )
  expect_equal(extract_fit_engine(cls_form)$W, extract_fit_engine(cls_xy_form)$W)

  expect_snapshot(
    error = TRUE,
    predict(cls_form, hpc_no_m[ind, -5], type = "prob")
  )

  expect_snapshot(
    error = TRUE,
    predict(cls_xy_form, hpc_no_m[ind, -5], type = "prob")
  )

})

# ------------------------------------------------------------------------------

reg_mod <-
  svm_linear(mode = "regression", cost = 1/4) |>
  set_engine("kernlab") |>
  set_mode("regression")

cls_mod <-
  svm_linear(mode = "classification", cost = 1/8) |>
  set_engine("kernlab") |>
  set_mode("classification")

ctrl <- control_parsnip(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('linear svm regression: kernlab', {

  skip_if_not_installed("kernlab")

  expect_no_condition(
    res <- fit_xy(
      reg_mod,
      control = ctrl,
      x = hpc[,2:4],
      y = hpc$input_fields
    )
  )
  expect_false(has_multi_predict(res))
  expect_equal(multi_predict_args(res), NA_character_)

  expect_no_condition(
    fit(
      reg_mod,
      input_fields ~ .,
      data = hpc[, -5],
      control = ctrl
    )
  )

})


test_that('linear svm regression prediction: kernlab', {

  skip_if_not_installed("kernlab")

  hpc_no_m <- hpc[-c(84, 85, 86, 87, 88, 109, 128),] |>
    droplevels()

  ind <- c(2, 1, 143)

  reg_form <-
    fit(
      reg_mod,
      input_fields ~ .,
      data = hpc[, -5],
      control = ctrl
    )

  kernlab_pred <-
    structure(
      list(.pred = c(129.9097, 376.1049, 1032.8989)),
      row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

  parsnip_pred <- predict(reg_form, hpc[ind, -c(2, 5)])
  expect_equal(as.data.frame(kernlab_pred),
               as.data.frame(parsnip_pred),
               tolerance = .0001)


  reg_xy_form <-
    fit_xy(
      reg_mod,
      x = hpc[, c(1, 3, 4)],
      y = hpc$input_fields,
      control = ctrl
    )
  expect_equal(extract_fit_engine(reg_form)@alphaindex, extract_fit_engine(reg_xy_form)@alphaindex)

  parsnip_xy_pred <- predict(reg_xy_form, hpc[ind, -c(2, 5)])
  expect_equal(as.data.frame(kernlab_pred),
               as.data.frame(parsnip_xy_pred),
               tolerance = .0001)
})

# ------------------------------------------------------------------------------

test_that('linear svm classification: kernlab', {

  skip_if_not_installed("kernlab")

  hpc_no_m <- hpc[-c(84, 85, 86, 87, 88, 109, 128),] |>
    droplevels()

  ind <- c(2, 1, 143)

  expect_no_condition(
    fit_xy(
      cls_mod,
      control = ctrl,
      x = hpc_no_m[, -5],
      y = hpc_no_m$class
    )
  )

  expect_no_condition(
    fit(
      cls_mod,
      class ~ .,
      data = hpc_no_m,
      control = ctrl
    )
  )

})


test_that('linear svm classification prediction: kernlab', {

  skip_if_not_installed("kernlab")

  hpc_no_m <- hpc[-c(84, 85, 86, 87, 88, 109, 128),] |>
    droplevels()

  ind <- c(4, 55, 143)

  set.seed(34562)
  cls_form <-
    fit(
      cls_mod,
      class ~ .,
      data = hpc_no_m,
      control = ctrl
    )

  kernlab_class <-
    structure(list(
      .pred_class = structure(
        c(1L, 1L, 3L),
        .Label = c("VF", "F", "L"), class = "factor")),
      row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

  parsnip_class <- predict(cls_form, hpc_no_m[ind, -5])
  expect_equal(kernlab_class, parsnip_class)

  set.seed(34562)
  cls_xy_form <-
    fit_xy(
      cls_mod,
      x = hpc_no_m[, 1:4],
      y = hpc_no_m$class,
      control = ctrl
    )
  expect_equal(extract_fit_engine(cls_form)@alphaindex, extract_fit_engine(cls_xy_form)@alphaindex)

  library(kernlab)
  kern_probs <-
    kernlab::predict(extract_fit_engine(cls_form), hpc_no_m[ind, -5], type = "probabilities") |>
    as_tibble() |>
    setNames(c('.pred_VF', '.pred_F', '.pred_L'))

  parsnip_probs <- predict(cls_form, hpc_no_m[ind, -5], type = "prob")
  expect_equal(as.data.frame(kern_probs), as.data.frame(parsnip_probs))

  parsnip_xy_probs <- predict(cls_xy_form, hpc_no_m[ind, -5], type = "prob")
  expect_equal(as.data.frame(kern_probs), as.data.frame(parsnip_xy_probs))

})

test_that("check_args() works", {
  # Here for completeness, no checking is done
  expect_true(TRUE)
})
