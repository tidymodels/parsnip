skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('updating', {
  expr1 <- mars(num_terms = 0) |> set_engine("earth", nk = 10)

  expect_snapshot(
    expr1 |>
      update(num_terms = tune(), nk = tune())
  )

  expect_equal(
    expr1 |>
      update(nk = tune()) |>
      extract_parameter_set_dials() |>
      nrow(),
    1
  )
})

test_that('bad input', {
  expect_snapshot(error = TRUE, translate(mars(mode = "regression") |> set_engine()))
  expect_snapshot(error = TRUE, translate(mars() |> set_engine("wat?")))
})

# ------------------------------------------------------------------------------

num_pred <- colnames(hpc)[1:3]
hpc_bad_form <- as.formula(class ~ term)
hpc_basic <- mars(mode = "regression") |> set_engine("earth")

# ------------------------------------------------------------------------------

test_that('mars execution', {
  skip_if_not_installed("earth")

  expect_no_condition(
    res <- fit(
      hpc_basic,
      compounds ~ log(input_fields) + class,
      data = hpc,
      control = ctrl
    )
  )

  expect_no_condition(
    res <- fit_xy(
      hpc_basic,
      x = hpc[, num_pred],
      y = hpc$num_pending,
      control = ctrl
    )
  )

  expect_true(has_multi_predict(res))
  expect_equal(multi_predict_args(res), "num_terms")

  ## multivariate y

  expect_no_condition(
    res <- fit(
      hpc_basic,
      cbind(compounds, input_fields) ~ .,
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
  parsnip:::load_libs(res, attach = TRUE)

})

test_that('mars prediction', {
  skip_if_not_installed("earth")

  uni_pred <- c(30.1466666666667, 30.1466666666667, 30.1466666666667,
                30.1466666666667, 30.1466666666667)
  inl_pred <- c(538.268789262046, 141.024903718634, 141.024903718634,
                141.024903718634, 141.024903718634)
  mv_pred <-
    structure(
      list(compounds =
             c(371.334864384913, 129.475162245595, 256.094366313268,
               129.475162245595, 129.475162245595),
           input_fields =
             c(430.476046435458, 158.833790342308, 218.07635084308,
               158.833790342308, 158.833790342308)
      ),
      class = "data.frame", row.names = c(NA, -5L)
    )

  res_xy <- fit_xy(
    hpc_basic,
    x = hpc[, num_pred],
    y = hpc$num_pending,
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
    cbind(compounds, input_fields) ~ .,
    data = hpc,
    control = ctrl
  )
  expect_equal(
    setNames(mv_pred, paste0(".pred_", names(mv_pred))) |> as.data.frame(),
    predict(res_mv, hpc[1:5,]) |> as.data.frame()
  )
})


test_that('submodel prediction', {
  skip_if_not_installed("earth")

  reg_fit <-
    mars(
      num_terms = 20,
      mode = "regression",
      prune_method = "none"
    ) |>
    set_engine("earth", keepxy = TRUE) |>
    fit(mpg ~ ., data = mtcars[-(1:4), ])

  parsnip:::load_libs(reg_fit$spec, quiet = TRUE, attach = TRUE)
  tmp_reg <- extract_fit_engine(reg_fit)
  tmp_reg$call[["pmethod"]] <- eval_tidy(tmp_reg$call[["pmethod"]])
  tmp_reg$call[["keepxy"]]  <- eval_tidy(tmp_reg$call[["keepxy"]])
  tmp_reg$call[["nprune"]]  <- eval_tidy(tmp_reg$call[["nprune"]])


  pruned_reg <- update(tmp_reg, nprune = 5)
  pruned_reg_pred <- predict(pruned_reg, mtcars[1:4, -1])[,1]

  mp_res <- multi_predict(reg_fit, new_data = mtcars[1:4, -1], num_terms = 5)
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred"]], pruned_reg_pred)

  full_churn <- wa_churn[complete.cases(wa_churn), ]
  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    mars(mode = "classification", prune_method = "none")  |>
    set_engine("earth", keepxy = TRUE) |>
    fit(churn ~ .,
        data = full_churn[-(1:4), c("churn", vars)])

  cls_fit <- extract_fit_engine(class_fit)
  cls_fit$call[["pmethod"]] <- eval_tidy(cls_fit$call[["pmethod"]])
  cls_fit$call[["keepxy"]]  <- eval_tidy(cls_fit$call[["keepxy"]])
  cls_fit$call[["glm"]]  <- eval_tidy(cls_fit$call[["glm"]])

  pruned_cls <- update(cls_fit, nprune = 5)
  pruned_cls_pred <- predict(pruned_cls, full_churn[1:4, vars], type = "response")[,1]

  mp_res <- multi_predict(class_fit, new_data = full_churn[1:4, vars], num_terms = 5, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_No"]], pruned_cls_pred)

  expect_snapshot(error = TRUE,
    multi_predict(reg_fit, newdata = mtcars[1:4, -1], num_terms = 5)
  )
})


# ------------------------------------------------------------------------------

test_that('classification', {
  skip_if_not_installed("earth")

  expect_no_condition(
    glm_mars <-
      mars(mode = "classification")  |>
      set_engine("earth") |>
      fit(Class ~ ., data = modeldata::lending_club[-(1:5),])
  )
  expect_true(!is.null(extract_fit_engine(glm_mars)$glm.list))
  parsnip_pred <- predict(glm_mars, new_data = lending_club[1:5, -ncol(lending_club)], type = "prob")

  earth_pred <-
    c(0.95631355972526, 0.971917781277731, 0.894245392500336, 0.962667553751077,
      0.985827594261896)

  expect_equal(parsnip_pred$.pred_good, earth_pred)
})

test_that("check_args() works", {
  skip_if_not_installed("earth")

  expect_snapshot(
    error = TRUE,
    {
      spec <- mars(prod_degree = 0) |>
        set_engine("earth") |>
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      spec <- mars(num_terms = 0) |>
        set_engine("earth") |>
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      spec <- mars(prune_method = 2) |>
        set_engine("earth") |>
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
})
