skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------
test_that('updating', {
  expect_snapshot(
    svm_poly(mode = "regression", degree = 2) |>
      set_engine("kernlab", cross = 10) |>
      update(degree = tune(), cross = tune())
  )
})

test_that('bad input', {
  expect_snapshot(error = TRUE, svm_poly(mode = "reallyunknown"))
  expect_snapshot(error = TRUE, svm_poly() |> set_engine(NULL) |> translate())
})

# ------------------------------------------------------------------------------

reg_mod <-
  svm_poly(mode = "regression", degree = 1, cost = 1/4) |>
  set_engine("kernlab") |>
  set_mode("regression")

cls_mod <-
  svm_poly(mode = "classification", degree = 2, cost = 1/8) |>
  set_engine("kernlab") |>
  set_mode("classification")

ctrl <- control_parsnip(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('svm poly regression', {

  skip_if_not_installed("kernlab")

  expect_no_condition(
    res <- fit_xy(
      reg_mod,
      control = ctrl,
      x = hpc[,2:4],
      y = hpc$compounds
    )
  )

  expect_false(has_multi_predict(res))
  expect_equal(multi_predict_args(res), NA_character_)

  expect_no_condition(
    fit(
      reg_mod,
      compounds ~ .,
      data = hpc[, -5],
      control = ctrl
    )
  )

})


test_that('svm poly regression prediction', {

  skip_if_not_installed("kernlab")

  reg_form <-
    fit(
      reg_mod,
      compounds ~ .,
      data = hpc[, -5],
      control = ctrl
    )

  # kern_pred <-
  #   predict(reg_form$fit, hpc[1:3, -c(1, 5)]) |>
  #   as_tibble() |>
  #   setNames(".pred")
  kern_pred <-
    structure(
      list(
        .pred = c(164.4739, 139.8284, 133.8760)),
      row.names = c(NA,-3L),
      class = c("tbl_df", "tbl", "data.frame")
    )

  parsnip_pred <- predict(reg_form, hpc[1:3, -c(1, 5)])
  expect_equal(as.data.frame(kern_pred),
               as.data.frame(parsnip_pred),
               tolerance = .0001)


  reg_xy_form <-
    fit_xy(
      reg_mod,
      x = hpc[, 2:4],
      y = hpc$compounds,
      control = ctrl
    )
  expect_equal(extract_fit_engine(reg_form)@alphaindex, extract_fit_engine(reg_xy_form)@alphaindex)

  parsnip_xy_pred <- predict(reg_xy_form, hpc[1:3, -c(1, 5)])
  expect_equal(as.data.frame(kern_pred),
               as.data.frame(parsnip_xy_pred),
               tolerance = .0001)
})

# ------------------------------------------------------------------------------

test_that('svm poly classification', {

  skip_if_not_installed("kernlab")

  expect_no_condition(
    fit_xy(
      cls_mod,
      control = ctrl,
      x = hpc[, -5],
      y = hpc$class
    )
  )

  expect_no_condition(
    fit(
      cls_mod,
      class ~ .,
      data = hpc,
      control = ctrl
    )
  )

})


test_that('svm poly classification probabilities', {

  skip_if_not_installed("kernlab")

  hpc_no_m <- hpc[-c(84, 85, 86, 87, 88, 109, 128),] |>
    droplevels()

  ind <- c(1, 2, 143)

  set.seed(34562)
  cls_form <-
    fit(
      cls_mod,
      class ~ .,
      data = hpc_no_m,
      control = ctrl
    )

  .pred_factor <- factor(c("F", "VF", "L"), levels = c("VF", "F", "L"))

  kern_class <-
    structure(
      list(
        .pred_class = .pred_factor),
      row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

  parsnip_class <- predict(cls_form, hpc_no_m[ind, -5])
  expect_equal(kern_class, parsnip_class)

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
