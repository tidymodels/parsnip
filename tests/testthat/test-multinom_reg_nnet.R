skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

set.seed(352)
dat <- hpc[order(runif(150)),]

tr_dat <- dat[1:140, ]
te_dat <- dat[141:150, ]

# ------------------------------------------------------------------------------

basic_mod <-
  multinom_reg() |>
  set_engine("nnet", penalty = .1)

ctrl <- control_parsnip(verbosity = 0, catch = FALSE)

# ------------------------------------------------------------------------------

test_that('model fitting', {
  skip_if_not_installed("nnet")

  set.seed(257)
  expect_no_condition(
    fit1 <-
      fit_xy(
        basic_mod,
        control = ctrl,
        x = tr_dat[, -5],
        y = tr_dat$class
      )
  )

  set.seed(257)
  expect_no_condition(
    fit2 <-
      fit_xy(
        basic_mod,
        control = ctrl,
        x = tr_dat[, -5],
        y = tr_dat$class
      )
  )
  fit1$elapsed <- fit2$elapsed
  expect_equal(fit1, fit2, ignore_formula_env = TRUE)

  expect_no_condition(
    fit(
      basic_mod,
      class ~ .,
      data = tr_dat,
      control = ctrl
    )
  )

})


test_that('classification prediction', {
  skip_if_not_installed("nnet")

  set.seed(257)
  lr_fit <-
    fit_xy(
      basic_mod,
      control = ctrl,
      x = tr_dat[, -5],
      y = tr_dat$class
    )

  nnet_pred <-
    predict(extract_fit_engine(lr_fit), as.matrix(te_dat[, -5]))

  parsnip_pred <- predict(lr_fit, te_dat[, -5])
  expect_equal(nnet_pred, parsnip_pred$.pred_class)

})


test_that('classification probabilities', {
  skip_if_not_installed("nnet")

  set.seed(257)
  lr_fit <-
    fit_xy(
      basic_mod,
      control = ctrl,
      x = tr_dat[, -5],
      y = tr_dat$class
    )

  nnet_pred <-
    predict(extract_fit_engine(lr_fit), as.matrix(te_dat[, -5]), type = "prob") |>
    as_tibble(.name_repair = "minimal") |>
    setNames(paste0(".pred_", lr_fit$lvl))

  parsnip_pred <- predict(lr_fit, te_dat[, -5], type = "prob")
  expect_equal(as.data.frame(nnet_pred), as.data.frame(parsnip_pred))

})

test_that('prob prediction with 1 row', {
  # For issue 612
  skip_if_not_installed("nnet")

  set.seed(257)
  lr_fit <-
    fit_xy(
      basic_mod,
      control = ctrl,
      x = tr_dat[, -5],
      y = tr_dat$class
    )

  nnet_pred <-
    predict(extract_fit_engine(lr_fit), as.matrix(te_dat[1, -5]), type = "prob") |>
    as.matrix() |>
    t() |>
    tibble::as_tibble(.name_repair = "minimal") |>
    setNames(paste0(".pred_", lr_fit$lvl))

  parsnip_pred <- predict(lr_fit, te_dat[1, -5], type = "prob")

  expect_equal(nnet_pred, parsnip_pred)
  expect_identical(nrow(parsnip_pred), 1L)
})


