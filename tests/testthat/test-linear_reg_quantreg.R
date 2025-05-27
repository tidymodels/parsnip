skip_if_not_installed("modeldata")

test_that('linear quantile regression via quantreg - single quantile', {
  skip_if_not_installed("quantreg")
  skip_if_not_installed("hardhat", minimum_version = "1.4.0.9002")

  # data in `helper-objects.R`

  one_quant <-
    linear_reg() |>
    set_engine("quantreg") |>
    set_mode("quantile regression", quantile_levels = .5) |>
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

  one_quant_df <- as_tibble(one_quant_pred$.pred_quantile)
  expect_s3_class(one_quant_df, c("tbl_df", "tbl", "data.frame"))
  expect_named(one_quant_df, c(".pred_quantile", ".quantile_levels", ".row"))
  expect_true(nrow(one_quant_df) == nrow(sac_test) * 1)

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

  one_quant_one_row_df <- as_tibble(one_quant_one_row$.pred_quantile)
  expect_s3_class(one_quant_one_row_df, c("tbl_df", "tbl", "data.frame"))
  expect_named(one_quant_one_row_df, c(".pred_quantile", ".quantile_levels", ".row"))
  expect_true(nrow(one_quant_one_row_df) == nrow(sac_test[1,]) * 1)
})

test_that('linear quantile regression via quantreg - multiple quantiles', {
  skip_if_not_installed("quantreg")

  # data in `helper-objects.R`

  ten_quant <-
    linear_reg() |>
    set_engine("quantreg") |>
    set_mode("quantile regression", quantile_levels = (0:9)/9) |>
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

  ten_quant_df <- as_tibble(ten_quant_pred$.pred_quantile)
  expect_s3_class(ten_quant_df, c("tbl_df", "tbl", "data.frame"))
  expect_named(ten_quant_df, c(".pred_quantile", ".quantile_levels", ".row"))
  expect_true(nrow(ten_quant_df) == nrow(sac_test) * 10)

  expect_snapshot(
    ten_quant_pred <- predict(ten_quant, new_data = sac_test, quantile_levels = (0:9)/9),
    error = TRUE
  )

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

  ten_quant_one_row_df <- as_tibble(ten_quant_one_row$.pred_quantile)
  expect_s3_class(ten_quant_one_row_df, c("tbl_df", "tbl", "data.frame"))
  expect_named(ten_quant_one_row_df, c(".pred_quantile", ".quantile_levels", ".row"))
  expect_true(nrow(ten_quant_one_row_df) == nrow(sac_test[1,]) * 10)
})



