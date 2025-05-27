skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]


num_pred <- c("compounds", "iterations", "num_pending")
hpc_bad_form <- as.formula(class ~ term)
hpc_basic <- nearest_neighbor(mode = "classification",
                               neighbors = 8,
                               weight_func = "triangular") |>
  set_engine("kknn")

# ------------------------------------------------------------------------------

test_that('kknn execution', {

  skip_if_not_installed("kknn")
  library(kknn)

  # nominal
  # expect no error
  expect_no_condition(
    res <- fit_xy(
      hpc_basic,
      control = ctrl,
      x = hpc[, c("input_fields", "iterations")],
      y = hpc$class
    )
  )

  expect_true(has_multi_predict(res))
  expect_equal(multi_predict_args(res), "neighbors")

  expect_snapshot(
    error = TRUE,
    fit(
      hpc_basic,
      hpc_bad_form,
      data = hpc,
      control = ctrl
    )
  )

})

test_that('kknn prediction', {

  skip_if_not_installed("kknn")
  library(kknn)

  # continuous
  res_xy <- fit_xy(
    hpc_basic,
    control = ctrl,
    x = hpc[, num_pred],
    y = hpc$class
  )

  uni_pred <- predict(
    extract_fit_engine(res_xy),
    newdata = hpc[1:5, num_pred]
  )

  expect_equal(tibble(.pred_class = uni_pred), predict(res_xy, hpc[1:5, num_pred]))

  # nominal
  res_xy_nom <- fit_xy(
    hpc_basic |> set_mode("classification"),
    control = ctrl,
    x = hpc[, c("input_fields", "iterations")],
    y = hpc$class
  )

  uni_pred_nom <- predict(
    extract_fit_engine(res_xy_nom),
    newdata = hpc[1:5, c("input_fields", "iterations")]
  )

  expect_equal(
    uni_pred_nom,
    predict(res_xy_nom, hpc[1:5, c("input_fields", "iterations")], type = "class")$.pred_class
  )

  # continuous - formula interface
  res_form <- fit(
    hpc_basic |> set_mode("regression"),
    input_fields ~ log(compounds) + class,
    data = hpc,
    control = ctrl
  )

  form_pred <- predict(
    extract_fit_engine(res_form),
    newdata = hpc[1:5,]
  )

  expect_equal(form_pred, predict(res_form, hpc[1:5, c("compounds", "class")])$.pred)
})


test_that('kknn multi-predict', {

  skip_if_not_installed("kknn")
  library(kknn)

  hpc_te <- c(1:2, 50:51, 100:101)
  k_vals <- 1:10

  res_xy <- fit_xy(
    nearest_neighbor(mode = "classification", neighbors = 3) |>
      set_engine("kknn"),
    control = ctrl,
    x = hpc[-hpc_te, num_pred],
    y = hpc$class[-hpc_te]
  )

  pred_multi <- multi_predict(res_xy, hpc[hpc_te, num_pred], neighbors = k_vals)
  expect_equal(pred_multi |> tidyr::unnest(cols = c(.pred)) |> nrow(),
               length(hpc_te) * length(k_vals))
  expect_equal(pred_multi |> nrow(), length(hpc_te))

  pred_uni <- predict(res_xy, hpc[hpc_te, num_pred])
  pred_uni_obs <-
    pred_multi |>
    dplyr::mutate(.rows = dplyr::row_number()) |>
    tidyr::unnest(cols = c(.pred)) |>
    dplyr::filter(neighbors == 3) |>
    dplyr::arrange(.rows) |>
    dplyr::select(.pred_class)
  expect_equal(pred_uni, pred_uni_obs)


  prob_multi <- multi_predict(res_xy, hpc[hpc_te, num_pred],
                              neighbors = k_vals, type = "prob")
  expect_equal(prob_multi |> tidyr::unnest(cols = c(.pred)) |> nrow(),
               length(hpc_te) * length(k_vals))
  expect_equal(prob_multi |> nrow(), length(hpc_te))

  prob_uni <- predict(res_xy, hpc[hpc_te, num_pred], type = "prob")
  prob_uni_obs <-
    prob_multi |>
    dplyr::mutate(.rows = dplyr::row_number()) |>
    tidyr::unnest(cols = c(.pred)) |>
    dplyr::filter(neighbors == 3) |>
    dplyr::arrange(.rows) |>
    dplyr::select(!!names(prob_uni))
  expect_equal(prob_uni, prob_uni_obs)

  # ----------------------------------------------------------------------------
  # regression

  cars_te <- 1:5
  k_vals <- 1:10

  res_xy <- fit(
    nearest_neighbor(mode = "regression", neighbors = 3) |>
      set_engine("kknn"),
    control = ctrl,
    mpg ~ ., data = mtcars[-cars_te, ]
  )

  pred_multi <- multi_predict(res_xy, mtcars[cars_te, -1], neighbors = k_vals)
  expect_equal(pred_multi |> tidyr::unnest(cols = c(.pred)) |> nrow(),
               length(cars_te) * length(k_vals))
  expect_equal(pred_multi |> nrow(), length(cars_te))

  pred_uni <- predict(res_xy, mtcars[cars_te, -1])
  pred_uni_obs <-
    pred_multi |>
    dplyr::mutate(.rows = dplyr::row_number()) |>
    tidyr::unnest(cols = c(.pred)) |>
    dplyr::filter(neighbors == 3) |>
    dplyr::arrange(.rows) |>
    dplyr::select(.pred)
  expect_equal(pred_uni, pred_uni_obs)
})


## -----------------------------------------------------------------------------

test_that('argument checks for data dimensions', {
  skip_if_not_installed("kknn")
  skip_if_not_installed("modeldata")

  data(penguins, package = "modeldata")
  penguins <- na.omit(penguins)

  spec <-
    nearest_neighbor(neighbors = 1000) |>
    set_engine("kknn") |>
    set_mode("regression")

  expect_snapshot(
    f_fit  <- spec |> fit(body_mass_g ~ ., data = penguins)
  )

  expect_snapshot(
    xy_fit <- spec |> fit_xy(x = penguins[, -6], y = penguins$body_mass_g)
  )

  expect_equal(extract_fit_engine(f_fit)$best.parameters$k,  nrow(penguins) - 5)
  expect_equal(extract_fit_engine(xy_fit)$best.parameters$k, nrow(penguins) - 5)

})

