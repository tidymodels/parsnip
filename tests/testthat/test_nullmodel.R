hpc <- hpc_data[1:150, c(2:5, 8)] %>% as.data.frame()

test_that('engine arguments', {
  nullmodel_keep <- null_model(mode = "regression")
  expect_equal(translate(nullmodel_keep %>% set_engine("parsnip", keepxy = FALSE))$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 keepxy = quo(FALSE)
               )
  )
})

test_that('bad input', {
  expect_error(translate(null_model(mode = "regression") %>% set_engine()))
  expect_error(translate(null_model() %>% set_engine("wat?")))
  expect_error(translate(null_model(formula = y ~ x)))
  expect_warning(
    translate(
      null_model(mode = "regression") %>% set_engine("parsnip", x = hpc[,1:3], y = hpc$class)
    )
  )
})

# ------------------------------------------------------------------------------

num_pred <- names(hpc)[1:3]
hpc_bad_form <- as.formula(class ~ term)
hpc_basic <- null_model(mode = "regression") %>% set_engine("parsnip")

# ------------------------------------------------------------------------------

test_that('nullmodel execution', {

  expect_error(
    res <- fit(
      hpc_basic,
      compounds ~ log(input_fields) + class,
      data = hpc
    ),
    regexp = NA
  )
  expect_error(
    res <- fit_xy(
      hpc_basic,
      x = hpc[, num_pred],
      y = hpc$num_pending
    ),
    regexp = NA
  )

  expect_error(
    res <- fit(
      hpc_basic,
      hpc_bad_form,
      data = hpc
    )
  )

  ## multivariate y

  expect_error(
    res <- fit(
      hpc_basic,
      cbind(compounds, input_fields) ~ .,
      data = hpc
    ),
    regexp = NA
  )

})

test_that('nullmodel prediction', {

  uni_pred <- tibble(.pred = rep(30.1, 5))
  inl_pred <- rep(30.1, 5)
  mw_pred <- tibble(gear = rep(3.6875, 5),
                    carb = rep(2.8125, 5))

  res_xy <- fit_xy(
    hpc_basic,
    x = hpc[, num_pred],
    y = hpc$num_pending
  )

  expect_equal(uni_pred,
               predict(res_xy, new_data = hpc[1:5, num_pred]),
               tolerance = .01)

  res_form <- fit(
    hpc_basic,
    num_pending ~ log(compounds) + class,
    data = hpc
  )
  expect_equal(inl_pred,
               predict(res_form, hpc[1:5, ])$.pred,
               tolerance = .01)

  # Multivariate y
  res <- fit(
    hpc_basic,
    cbind(gear, carb) ~ .,
    data = mtcars
  )

  expect_equal(
    setNames(mw_pred, paste0(".pred_", names(mw_pred))),
    predict(res, mtcars[1:5, ])
  )
})

# ------------------------------------------------------------------------------

test_that('classification', {

  expect_error(
    null_model <- null_model(mode = "classification") %>%
      set_engine("parsnip") %>%
      fit(class ~ ., data = hpc),
    regexp = NA
  )
  expect_true(!is.null(null_model$fit))
})


