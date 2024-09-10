test_that('linear quantile regression via quantreg - single quantile', {
  skip_if_not_installed("quantreg")

  data("Sacramento")

  Sacramento_small <-
    Sacramento %>%
    dplyr::select(price, beds, baths, sqft, latitude, longitude)

  sac_train <- Sacramento_small[-(1:5), ]
  sac_test  <- Sacramento_small[  1:5 , ]

  one_quant <-
    linear_reg() %>%
    set_engine("quantreg") %>%
    set_mode("quantile regression", quantile_level = .5) %>%
    fit(price ~ ., data = sac_train)

  expect_s3_class(one_quant, c("_rq", "model_fit"))

  ###

  one_quant_pred <- predict(one_quant, new_data = sac_test)
  expect_true(nrow(one_quant_pred) == nrow(sac_test))
  expect_named(one_quant_pred, ".pred_quantile")
  expect_true(is.list(one_quant_pred[[1]]))
  expect_s3_class(
    one_quant_pred$.pred_quantile[1],
    c("quantile_pred", "vctrs_vctr", "list")
  )
  expect_identical(class(one_quant_pred$.pred_quantile[[1]]), "numeric")
  expect_true(length(one_quant_pred$.pred_quantile[[1]]) == 1L)
  expect_identical(attr(one_quant_pred$.pred_quantile, "quantile_levels"), .5)

  ###

  one_quant_one_row <- predict(one_quant, new_data = sac_test[1,])
  expect_true(nrow(one_quant_one_row) == 1L)
  expect_named(one_quant_one_row, ".pred_quantile")
  expect_true(is.list(one_quant_one_row[[1]]))
  expect_s3_class(
    one_quant_one_row$.pred_quantile[1],
    c("quantile_pred", "vctrs_vctr", "list")
  )
  expect_identical(class(one_quant_one_row$.pred_quantile[[1]]), "numeric")
  expect_true(length(one_quant_one_row$.pred_quantile[[1]]) == 1L)
  expect_identical(attr(one_quant_pred$.pred_quantile, "quantile_levels"), .5)
})

test_that('linear quantile regression via quantreg - multiple quantiles', {
  skip_if_not_installed("quantreg")

  data("Sacramento")

  Sacramento_small <-
    Sacramento %>%
    dplyr::select(price, beds, baths, sqft, latitude, longitude)

  sac_train <- Sacramento_small[-(1:5), ]
  sac_test  <- Sacramento_small[  1:5 , ]

  ten_quant <-
    linear_reg() %>%
    set_engine("quantreg") %>%
    set_mode("quantile regression", quantile_level = (0:9)/9) %>%
    fit(price ~ ., data = sac_train)

  expect_s3_class(ten_quant, c("_rq", "model_fit"))

  ###

  ten_quant_pred <- predict(ten_quant, new_data = sac_test)
  expect_true(nrow(ten_quant_pred) == nrow(sac_test))
  expect_named(ten_quant_pred, ".pred_quantile")
  expect_true(is.list(ten_quant_pred[[1]]))
  expect_s3_class(
    ten_quant_pred$.pred_quantile[1],
    c("quantile_pred", "vctrs_vctr", "list")
  )
  expect_identical(class(ten_quant_pred$.pred_quantile[[1]]), "numeric")
  expect_true(length(ten_quant_pred$.pred_quantile[[1]]) == 10L)
  expect_identical(attr(ten_quant_pred$.pred_quantile, "quantile_levels"), (0:9)/9)

  ###

  ten_quant_one_row <- predict(ten_quant, new_data = sac_test[1,])
  expect_true(nrow(ten_quant_one_row) == 1L)
  expect_named(ten_quant_one_row, ".pred_quantile")
  expect_true(is.list(ten_quant_one_row[[1]]))
  expect_s3_class(
    ten_quant_one_row$.pred_quantile[1],
    c("quantile_pred", "vctrs_vctr", "list")
  )
  expect_identical(class(ten_quant_one_row$.pred_quantile[[1]]), "numeric")
  expect_true(length(ten_quant_one_row$.pred_quantile[[1]]) == 10L)
  expect_identical(
    attr(ten_quant_one_row$.pred_quantile, "quantile_levels"),
    (0:9)/9
  )
})



