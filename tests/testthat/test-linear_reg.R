skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('updating', {
  expect_snapshot(
    linear_reg(mixture = 0) |>
      set_engine("glmnet", nlambda = 10) |>
      update(mixture = tune(), nlambda = tune())
  )
})

test_that('bad input', {
  expect_snapshot(error = TRUE, linear_reg(mode = "classification"))
  expect_snapshot(error = TRUE, translate(linear_reg(), engine = "wat?"))
  expect_snapshot(error = TRUE, translate(linear_reg(), engine = NULL))
})

# ------------------------------------------------------------------------------

num_pred <- c("input_fields", "num_pending", "iterations")
hpc_bad_form <- as.formula(class ~ term)
hpc_basic <- linear_reg() |> set_engine("lm")

# ------------------------------------------------------------------------------

test_that('lm execution', {

  expect_no_condition(
    res <- fit(
      hpc_basic,
      input_fields ~ log(compounds) + class,
      data = hpc,
      control = ctrl
    )
  )

  expect_no_condition(
    res <- fit_xy(
      hpc_basic,
      x = hpc[, num_pred],
      y = hpc$input_fields,
      control = ctrl
    )
  )

  expect_snapshot(
    error = TRUE,
    res <- fit_xy(
      hpc_basic,
      x = hpc[, num_pred],
      y = hpc$class,
      control = ctrl
    )
  )

  expect_snapshot(
    error = TRUE,
    res <- fit_xy(
      hpc_basic,
      x = hpc[, num_pred],
      y = as.character(hpc$class),
      control = ctrl
    )
  )

  expect_snapshot(
    error = TRUE,
    res <- fit(
      hpc_basic,
      hpc_bad_form,
      data = hpc,
      control = ctrl
    )
  )

  expect_snapshot(
    error = TRUE,
    lm_form_catch <- fit(
      hpc_basic,
      hpc_bad_form,
      data = hpc,
      control = caught_ctrl
    )
  )

  ## multivariate y

  expect_no_condition(
    res <- fit(
      hpc_basic,
      cbind(compounds, iterations) ~ .,
      data = hpc,
      control = ctrl
    )
  )

  expect_no_condition(
    res <- fit_xy(
      hpc_basic,
      x = hpc[, 1:2],
      y = hpc[3:4],
      control = ctrl
    )
  )
})

test_that('glm execution', {

  hpc_glm <- linear_reg() |> set_engine("glm")

  expect_no_condition(
    res <- fit(
      hpc_glm,
      input_fields ~ log(compounds) + class,
      data = hpc,
      control = ctrl
    )
  )

  expect_no_condition(
    res <- fit_xy(
      hpc_glm,
      x = hpc[, num_pred],
      y = hpc$input_fields,
      control = ctrl
    )
  )

  expect_snapshot(
    error = TRUE,
    res <- fit_xy(
      hpc_glm,
      x = hpc[, num_pred],
      y = hpc$class,
      control = ctrl
    )
  )

  expect_snapshot(
    error = TRUE,
    res <- fit(
      hpc_glm,
      hpc_bad_form,
      data = hpc,
      control = ctrl
    )
  )

  expect_snapshot(
    error = TRUE,
    lm_form_catch <- fit(
      hpc_glm,
      hpc_bad_form,
      data = hpc,
      control = caught_ctrl
    )
  )

})


test_that('lm prediction', {
  uni_lm <- lm(compounds ~ input_fields + num_pending + iterations, data = hpc)
  uni_pred <- unname(predict(uni_lm, newdata = hpc[1:5, ]))
  inl_lm <- lm(compounds ~ log(input_fields) + class, data = hpc)
  inl_pred <- unname(predict(inl_lm, newdata = hpc[1:5, ]))
  mv_lm <- lm(cbind(input_fields, num_pending) ~ ., data = hpc)
  mv_pred <- tibble::as_tibble(predict(mv_lm, newdata = hpc[1:5, ]))
  names(mv_pred) <- c(".pred_input_fields", ".pred_num_pending")

  res_xy <- fit_xy(
    hpc_basic,
    x = hpc[, num_pred],
    y = hpc$compounds,
    control = ctrl
  )

  expect_equal(uni_pred, predict(res_xy, hpc[1:5, num_pred])$.pred)

  res_form <- fit(
    hpc_basic,
    compounds ~ log(input_fields) + class,
    data = hpc,
    control = ctrl
  )

  expect_equal(inl_pred, predict(res_form, hpc[1:5, ])$.pred)

  res_mv <- fit(
    hpc_basic,
    cbind(input_fields, num_pending) ~ .,
    data = hpc,
    control = ctrl
  )

  expect_equal(mv_pred, predict(res_mv, hpc[1:5,]))
})

test_that('glm prediction', {

  hpc_glm <- linear_reg() |> set_engine("glm")

  uni_lm <- glm(compounds ~ input_fields + num_pending + iterations, data = hpc)
  uni_pred <- unname(predict(uni_lm, newdata = hpc[1:5, ]))
  inl_lm <- glm(compounds ~ log(input_fields) + class, data = hpc)
  inl_pred <- unname(predict(inl_lm, newdata = hpc[1:5, ]))

  res_xy <- fit_xy(
    hpc_glm,
    x = hpc[, num_pred],
    y = hpc$compounds,
    control = ctrl
  )

  expect_equal(uni_pred, predict(res_xy, hpc[1:5, num_pred])$.pred)

  res_form <- fit(
    hpc_glm,
    compounds ~ log(input_fields) + class,
    data = hpc,
    control = ctrl
  )

  expect_equal(inl_pred, predict(res_form, hpc[1:5, ])$.pred)

})

test_that('lm intervals', {
  stats_lm <- lm(compounds ~ input_fields + iterations + num_pending,
                 data = hpc)
  confidence_lm <- predict(stats_lm, newdata = hpc[1:5, ],
                           level = 0.93, interval = "confidence")
  prediction_lm <- predict(stats_lm, newdata = hpc[1:5, ],
                           level = 0.93, interval = "prediction")

  res_xy <- fit_xy(
    linear_reg()  |> set_engine("lm"),
    x = hpc[, num_pred],
    y = hpc$compounds,
    control = ctrl
  )

  confidence_parsnip <-
    predict(res_xy,
            new_data = hpc[1:5,],
            type = "conf_int",
            level = 0.93)

  expect_equal(confidence_parsnip$.pred_lower, confidence_lm[, "lwr"], ignore_attr = TRUE)
  expect_equal(confidence_parsnip$.pred_upper, confidence_lm[, "upr"], ignore_attr = TRUE)

  prediction_parsnip <-
    predict(res_xy,
            new_data = hpc[1:5,],
            type = "pred_int",
            level = 0.93)

  expect_equal(prediction_parsnip$.pred_lower, prediction_lm[, "lwr"], ignore_attr = TRUE)
  expect_equal(prediction_parsnip$.pred_upper, prediction_lm[, "upr"], ignore_attr = TRUE)
})

test_that('glm intervals', {
  stats_glm <- glm(compounds ~ input_fields + iterations + num_pending,
                   data = hpc)
  pred_glm <- predict(stats_glm, newdata = hpc[1:5, ], se.fit = TRUE)
  t_val <- qt(0.035, df = stats_glm$df.residual, lower.tail = FALSE)
  lower_glm <- pred_glm$fit - t_val * pred_glm$se.fit
  upper_glm <- pred_glm$fit + t_val * pred_glm$se.fit

  lower_glm <- stats_glm$family$linkinv(lower_glm)
  upper_glm <- stats_glm$family$linkinv(upper_glm)

  res_xy <- fit_xy(
    linear_reg()  |> set_engine("glm"),
    x = hpc[, num_pred],
    y = hpc$compounds,
    control = ctrl
  )

  confidence_parsnip <-
    predict(res_xy,
            new_data = hpc[1:5,],
            type = "conf_int",
            level = 0.93)

  expect_equal(confidence_parsnip$.pred_lower, lower_glm)
  expect_equal(confidence_parsnip$.pred_upper, upper_glm)

})


test_that('newdata error trapping', {
  res_xy <- fit_xy(
    hpc_basic,
    x = hpc[, num_pred],
    y = hpc$input_fields,
    control = ctrl
  )
  expect_snapshot(error = TRUE,
    predict(res_xy, newdata = hpc[1:3, num_pred])
  )
})

test_that('show engine', {
  res <- show_engines("linear_reg")
  expt <- get_from_env("linear_reg")
  expect_equal(res, expt)
  expect_snapshot(error = TRUE, show_engines("linear_re"))
})

test_that('lm can handle rankdeficient predictions', {
  skip_if(
    paste0(R.Version()[c("major", "minor")], collapse = ".") < "4.3.0",
    "R doesn't raise the rank-deficient warning in this R version"
  )

  data <- data.frame(
    y = c(1,2,3,4),
    x1 = c(1,1,2,3),
    x2 = c(3,4,5,2),
    x3 = c(4,2,6,0),
    x4 = c(2,1,3,0)
  )
  data2 <- data.frame(
    x1 = c(3,2,1,3),
    x2 = c(3,2,1,4),
    x3 = c(3,4,5,1),
    x4 = c(0,0,2,3)
  )

  expect_snapshot(
    preds <- linear_reg() |>
      fit(y ~ ., data = data) |>
      predict(new_data = data2)
  )

  expect_identical(names(preds), ".pred")
})

test_that("check_args() works", {
  expect_snapshot(
    error = TRUE,
    {
      spec <- linear_reg(mixture = -1) |>
        set_engine("lm") |>
        set_mode("regression")
      fit(spec, compounds ~ ., hpc)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      spec <- linear_reg(penalty = -1) |>
        set_engine("lm") |>
        set_mode("regression")
      fit(spec, compounds ~ ., hpc)
    }
  )
})


test_that("prevent using a Poisson family", {
  skip_if_not_installed("glmnet")

  expect_snapshot(
    linear_reg(penalty = 1) |>
      set_engine("glmnet", family = poisson) |>
      fit(mpg ~ ., data = mtcars),
    error = TRUE
  )
  expect_snapshot(
    linear_reg(penalty = 1) |>
      set_engine("glmnet", family = stats::poisson) |>
      fit(mpg ~ ., data = mtcars),
    error = TRUE
  )
  expect_snapshot(
    linear_reg(penalty = 1) |>
      set_engine("glmnet", family = stats::poisson()) |>
      fit(mpg ~ ., data = mtcars),
    error = TRUE
  )
  expect_snapshot(
    linear_reg(penalty = 1) |>
      set_engine("glmnet", family = "poisson") |>
      fit(mpg ~ ., data = mtcars),
    error = TRUE
  )
})


# ------------------------------------------------------------------------------

test_that("tunables", {

  expect_snapshot(
    linear_reg() |>
      tunable()
  )

  expect_snapshot(
    linear_reg() |>
      set_engine("brulee") |>
      tunable()
  )
  expect_snapshot(
    linear_reg() |>
      set_engine("glmnet") |>
      tunable()
  )

  expect_snapshot(
    linear_reg() |>
      set_engine("quantreg") |>
      tunable()
  )

  expect_snapshot(
    linear_reg() |>
      set_engine("keras") |>
      tunable()
  )

})

