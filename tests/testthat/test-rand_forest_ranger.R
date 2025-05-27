skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

lending_club <- head(lending_club, 200)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")

lc_basic <- rand_forest(mode = "classification") |> set_engine("ranger")
lc_ranger <- rand_forest(mode = "classification") |> set_engine("ranger", seed = 144)

bad_ranger_cls <- rand_forest(mode = "classification") |> set_engine("ranger", replace = "bad")
bad_rf_cls <- rand_forest(mode = "classification") |> set_engine("ranger", sampsize = -10)

# ------------------------------------------------------------------------------

test_that('ranger classification execution', {

  skip_if_not_installed("ranger")

  expect_no_condition(
    res <- fit(
      lc_ranger,
      Class ~ funded_amnt + term,
      data = lending_club,
      control = ctrl
    )
  )

  expect_snapshot(
    error = TRUE,
    res <- fit(
      lc_ranger,
      funded_amnt ~ Class + term,
      data = lending_club,
      control = ctrl
    )
  )

  expect_no_condition(
    res <- fit_xy(
      lc_ranger,
      x = lending_club[, num_pred],
      y = lending_club$Class,

      control = ctrl
    )
  )

  ranger_form_catch <- fit(
    bad_ranger_cls,
    Class ~ term,
    data = lending_club,

    control = caught_ctrl
  )
  expect_true(inherits(extract_fit_engine(ranger_form_catch), "try-error"))

  ranger_xy_catch <- fit_xy(
    bad_ranger_cls,

    control = caught_ctrl,
    x = lending_club[, num_pred],
    y = lending_club$Class
  )
  expect_true(inherits(extract_fit_engine(ranger_xy_catch), "try-error"))

})

test_that('ranger classification prediction', {

  skip_if_not_installed("ranger")

  xy_fit <- fit_xy(
    rand_forest() |> set_mode("classification") |> set_engine("ranger"),
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- predict(extract_fit_engine(xy_fit), data = lending_club[1:6, num_pred])$prediction
  xy_pred <- colnames(xy_pred)[apply(xy_pred, 1, which.max)]
  xy_pred <- factor(xy_pred, levels = levels(lending_club$Class))
  expect_equal(
    xy_pred,
    predict(xy_fit, new_data = lending_club[1:6, num_pred], type = "class")$.pred_class
  )

  form_fit <- fit(
    rand_forest() |> set_mode("classification") |> set_engine("ranger"),
    Class ~ funded_amnt + int_rate,
    data = lending_club,

    control = ctrl
  )

  form_pred <- predict(extract_fit_engine(form_fit), data = lending_club[1:6, c("funded_amnt", "int_rate")])$prediction
  form_pred <- colnames(form_pred)[apply(form_pred, 1, which.max)]
  form_pred <- factor(form_pred, levels = levels(lending_club$Class))
  expect_equal(
    form_pred,
    predict(form_fit, new_data = lending_club[1:6, c("funded_amnt", "int_rate")])$.pred_class
  )

})


test_that('ranger classification probabilities', {

  skip_if_not_installed("ranger")

  xy_fit <- fit_xy(
    rand_forest() |> set_mode("classification") |> set_engine("ranger", seed = 3566),
    x = lending_club[, num_pred],
    y = lending_club$Class,

    control = ctrl
  )

  xy_pred <- predict(extract_fit_engine(xy_fit), data = lending_club[1:6, num_pred])$predictions
  xy_pred <- as_tibble(xy_pred)
  names(xy_pred) <- paste0(".pred_", names(xy_pred))
  expect_equal(
    xy_pred,
    predict(xy_fit, new_data = lending_club[1:6, num_pred], type = "prob")
  )

  one_row <- predict(xy_fit, new_data = lending_club[1, num_pred], type = "prob")
  expect_equal(xy_pred[1,], one_row)

  form_fit <- fit(
    rand_forest() |> set_mode("classification") |> set_engine("ranger", seed = 3566),
    Class ~ funded_amnt + int_rate,
    data = lending_club,

    control = ctrl
  )

  form_pred <- predict(extract_fit_engine(form_fit), data = lending_club[1:6, c("funded_amnt", "int_rate")])$predictions
  form_pred <- as_tibble(form_pred)
  names(form_pred) <- paste0(".pred_", names(form_pred))
  expect_equal(
    form_pred,
    predict(form_fit, new_data = lending_club[1:6, c("funded_amnt", "int_rate")], type = "prob")
  )

  no_prob_model <- fit_xy(
    rand_forest(mode = "classification") |> set_engine("ranger", probability = FALSE),
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  expect_snapshot(
    error = TRUE,
    parsnip:::predict_classprob.model_fit(no_prob_model, new_data = lending_club[1:6, num_pred])
  )
})

# ------------------------------------------------------------------------------

num_pred <- names(mtcars)[3:6]

car_basic <- rand_forest(mode = "regression") |> set_engine("ranger")

bad_ranger_reg <- rand_forest(mode = "regression") |> set_engine("ranger", replace = "bad")
bad_rf_reg <- rand_forest(mode = "regression") |> set_engine("ranger", sampsize = -10)

# ------------------------------------------------------------------------------

test_that('ranger regression execution', {

  skip_if_not_installed("ranger")

  expect_no_condition(
    res <- fit(
      car_basic,
      mpg ~ .,
      data = mtcars,
      control = ctrl
    )
  )

  expect_no_condition(
    res <- fit_xy(
      car_basic,
      x = mtcars,
      y = mtcars$mpg,
      control = ctrl
    )
  )


  ranger_form_catch <- fit(
    bad_ranger_reg,
    mpg ~ .,
    data = mtcars,
    control = caught_ctrl
  )
  expect_true(inherits(extract_fit_engine(ranger_form_catch), "try-error"))

  ranger_xy_catch <- fit_xy(
    bad_ranger_reg,
    control = caught_ctrl,
    x = mtcars[, num_pred],
    y = mtcars$mpg
  )
  expect_true(inherits(extract_fit_engine(ranger_xy_catch), "try-error"))

})

test_that('ranger regression prediction', {

  skip_if_not_installed("ranger")

  xy_fit <- fit_xy(
    car_basic,
    x = mtcars[, -1],
    y = mtcars$mpg,
    control = ctrl
  )

  xy_pred <- predict(extract_fit_engine(xy_fit), data = tail(mtcars[, -1]))$prediction

  expect_equal(xy_pred, predict(xy_fit, new_data = tail(mtcars[, -1]))$.pred)

})


test_that('ranger regression intervals', {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("ranger")

  data(ames, package = "modeldata")
  ames$Sale_Price <- log10(ames$Sale_Price)
  ames_x <- ames[, c("Longitude", "Latitude")]

  set.seed(1)
  xy_fit <- fit_xy(
    rand_forest(mode = "regression") |> set_engine("ranger", keep.inbag = TRUE),
    x = ames_x[-(1:3), ],
    y = ames$Sale_Price,
    control = ctrl
  )

  rgr_pred <- predict(extract_fit_engine(xy_fit), data = head(ames_x, 3))$predictions
  expect_snapshot(
    rgr_se <-
      predict(extract_fit_engine(xy_fit), data = head(ames_x, 3), type = "se")$se
  )
  rgr_lower <- rgr_pred - qnorm(0.035, lower.tail = FALSE) * rgr_se
  rgr_upper <- rgr_pred + qnorm(0.035, lower.tail = FALSE) * rgr_se

  expect_snapshot(
    parsnip_int <-
      predict(xy_fit, new_data = head(ames_x, 3),
              type = "conf_int", std_error = TRUE, level = 0.93
      )
  )
  expect_equal(rgr_lower, parsnip_int$.pred_lower, ignore_formula_env = TRUE)
  expect_equal(rgr_upper, parsnip_int$.pred_upper)
  expect_equal(rgr_se, parsnip_int$.std_error)

})



test_that('additional descriptor tests', {

  skip_if_not_installed("ranger")

  descr_xy <- fit_xy(
    rand_forest(mode = "regression", mtry = floor(sqrt(.cols())) + 1) |>
      set_engine("ranger"),
    x = mtcars[, -1],
    y = mtcars$mpg,
    control = ctrl
  )
  expect_equal(extract_fit_engine(descr_xy)$mtry, 4)

  descr_f <- fit(
    rand_forest(mode = "regression", mtry = floor(sqrt(.cols())) + 1) |>
      set_engine("ranger"),
    mpg ~ ., data = mtcars,
    control = ctrl
  )
  expect_equal(extract_fit_engine(descr_f)$mtry, 4)

  descr_xy <- fit_xy(
    rand_forest(mode = "regression", mtry = floor(sqrt(.cols())) + 1) |>
      set_engine("ranger"),
    x = mtcars[, -1],
    y = mtcars$mpg,
    control = ctrl
  )
  expect_equal(extract_fit_engine(descr_xy)$mtry, 4)

  descr_f <- fit(
    rand_forest(mode = "regression", mtry = floor(sqrt(.cols())) + 1) |>
      set_engine("ranger"),
    mpg ~ ., data = mtcars,
    control = ctrl
  )
  expect_equal(extract_fit_engine(descr_f)$mtry, 4)

  ##

  exp_wts <- rlang::quo(c(min(.lvls()), 20, 10, 1))

  descr_other_xy <- fit_xy(
    rand_forest(mode = "classification", mtry = 2) |>
      set_engine("ranger", class.weights = c(min(.lvls()), 20, 10, 1)),
    x = hpc[, 1:4],
    y = hpc$class,
    control = ctrl
  )
  expect_equal(extract_fit_engine(descr_other_xy)$mtry, 2)
  expect_equal(extract_fit_engine(descr_other_xy)$call$class.weights, exp_wts,
               ignore_formula_env = TRUE)

  descr_other_f <- fit(
    rand_forest(mode = "classification", mtry = 2) |>
      set_engine("ranger", class.weights = c(min(.lvls()), 20, 10, 1)),
    class ~ ., data = hpc,
    control = ctrl
  )
  expect_equal(extract_fit_engine(descr_other_f)$mtry, 2)
  expect_equal(extract_fit_engine(descr_other_f)$call$class.weights, exp_wts,
               ignore_formula_env = TRUE)

  descr_other_xy <- fit_xy(
    rand_forest(mode = "classification", mtry = 2) |>
      set_engine("ranger", class.weights = c(min(.lvls()), 20, 10, 1)),
    x = hpc[, 1:4],
    y = hpc$class,
    control = ctrl
  )
  expect_equal(extract_fit_engine(descr_other_xy)$mtry, 2)
  expect_equal(extract_fit_engine(descr_other_xy)$call$class.weights, exp_wts,
               ignore_formula_env = TRUE)

  descr_other_f <- fit(
    rand_forest(mode = "classification", mtry = 2) |>
      set_engine("ranger", class.weights = c(min(.lvls()), 20, 10, 1)),
    class ~ ., data = hpc,
    control = ctrl
  )
  expect_equal(extract_fit_engine(descr_other_f)$mtry, 2)
  expect_equal(extract_fit_engine(descr_other_f)$call$class.weights, exp_wts,
               ignore_formula_env = TRUE)
})


test_that('ranger classification prediction', {

  skip_if_not_installed("ranger")

  xy_class_fit <-
    rand_forest() |> set_mode("classification")  |> set_engine("ranger") |>
    fit_xy(
      x = hpc[, 1:4],
      y = hpc$class,
      control = ctrl
    )

  expect_false(has_multi_predict(xy_class_fit))
  expect_equal(multi_predict_args(xy_class_fit), NA_character_)

  xy_class_pred <- predict(extract_fit_engine(xy_class_fit), data = hpc[c(1, 51, 101), 1:4])$prediction
  xy_class_pred <- colnames(xy_class_pred)[apply(xy_class_pred, 1, which.max)]
  xy_class_pred <- factor(xy_class_pred, levels = levels(hpc$class))

  expect_equal(
    xy_class_pred,
    predict(xy_class_fit, new_data = hpc[c(1, 51, 101), 1:4])$.pred_class
  )

  xy_prob_fit <-
    rand_forest() |>
    set_mode("classification") |>
    set_engine("ranger") |>
    fit_xy(
      x = hpc[, 1:4],
      y = hpc$class,
      control = ctrl
    )

  xy_prob_pred <- predict(extract_fit_engine(xy_prob_fit), data = hpc[c(1, 51, 101), 1:4])$prediction
  xy_prob_pred <- colnames(xy_prob_pred)[apply(xy_prob_pred, 1, which.max)]
  xy_prob_pred <- factor(xy_prob_pred, levels = levels(hpc$class))

  expect_equal(
    xy_class_pred,
    predict(xy_prob_fit, new_data = hpc[c(1, 51, 101), 1:4])$.pred_class
  )

  xy_prob_prob <- predict(extract_fit_engine(xy_prob_fit), data = hpc[c(1, 51, 101), 1:4], type = "response")
  xy_prob_prob <- as_tibble(xy_prob_prob$prediction)
  names(xy_prob_prob) <- paste0(".pred_", names(xy_prob_prob))
  expect_equal(
    xy_prob_prob,
    predict(xy_prob_fit, new_data = hpc[c(1, 51, 101), 1:4], type = "prob")
  )
})


test_that('ranger classification intervals', {

  skip_if_not_installed("ranger")

  lc_fit <- fit(
    rand_forest(mode = "classification") |>
      set_engine("ranger", keep.inbag = TRUE, probability = TRUE),
    Class ~ funded_amnt + int_rate,
    data = lending_club,
    control = ctrl
  )

  rgr_pred <- predict(extract_fit_engine(lc_fit), data = tail(lending_club))$predictions
  expect_snapshot(
    rgr_se <- predict(extract_fit_engine(lc_fit), data = tail(lending_club), type = "se")$se
  )
  rgr_lower <- rgr_pred - qnorm(0.035, lower.tail = FALSE) * rgr_se
  rgr_upper <- rgr_pred + qnorm(0.035, lower.tail = FALSE) * rgr_se
  rgr_lower[rgr_lower < 0] <- 0
  rgr_upper[rgr_upper > 1] <- 1

  expect_snapshot(
    parsnip_int <-
      predict(lc_fit, new_data = tail(lending_club),
              type = "conf_int", std_error = TRUE, level = 0.93
      )
  )
  expect_equal(rgr_lower[, "bad"], parsnip_int$.pred_lower_bad)
  expect_equal(rgr_lower[, "good"], parsnip_int$.pred_lower_good)
  expect_equal(rgr_upper[, "bad"], parsnip_int$.pred_upper_bad)
  expect_equal(rgr_upper[, "good"], parsnip_int$.pred_upper_good)
  expect_equal(rgr_se[, 1], parsnip_int$.std_error_bad)
  expect_equal(rgr_se[, 2], parsnip_int$.std_error_good)

})



test_that('ranger and sparse matrices', {
  skip_if_not_installed("ranger")

  mtcar_x <- mtcars[, -1]
  mtcar_mat <- as.matrix(mtcar_x)
  mtcar_smat <- Matrix::Matrix(mtcar_mat, sparse = TRUE)

  rf_spec <-
    rand_forest(trees = 10) |>
    set_engine("ranger", seed = 2) |>
    set_mode("regression")

  set.seed(1)
  from_df <- rf_spec |> fit_xy(mtcar_x, mtcars$mpg)
  set.seed(1)
  from_mat <- rf_spec |> fit_xy(mtcar_mat, mtcars$mpg)
  set.seed(1)
  from_sparse <- rf_spec |> fit_xy(mtcar_smat, mtcars$mpg)

  expect_equal(extract_fit_engine(from_df), extract_fit_engine(from_mat))
  expect_equal(extract_fit_engine(from_df), extract_fit_engine(from_sparse))
})


## -----------------------------------------------------------------------------

test_that('argument checks for data dimensions', {

  skip_if_not_installed("ranger")

  data(penguins, package = "modeldata")
  penguins <- na.omit(penguins)

  spec <-
    rand_forest(mtry = 1000, min_n = 1000, trees = 5) |>
    set_engine("ranger") |>
    set_mode("regression")

  expect_snapshot(
    f_fit  <- spec |> fit(body_mass_g ~ ., data = penguins)
  )
  expect_snapshot(
    xy_fit <- spec |> fit_xy(x = penguins[, -6], y = penguins$body_mass_g)
  )


  expect_equal(extract_fit_engine(f_fit)$mtry, 6)
  expect_equal(extract_fit_engine(f_fit)$min.node.size, nrow(penguins))
  expect_equal(extract_fit_engine(xy_fit)$mtry, 6)
  expect_equal(extract_fit_engine(xy_fit)$min.node.size, nrow(penguins))

})
