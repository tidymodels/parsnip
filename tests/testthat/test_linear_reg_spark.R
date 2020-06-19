library(testthat)
library(parsnip)
library(dplyr)

# ------------------------------------------------------------------------------

context("linear regression execution with spark")
source(test_path("helper-objects.R"))
hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('spark execution', {

  skip_if_not_installed("sparklyr")

  library(sparklyr)

  sc <- try(spark_connect(master = "local"), silent = TRUE)

  skip_if(inherits(sc, "try-error"))

  hpc_linreg_tr <- copy_to(sc, hpc[-(1:4),   ], "hpc_linreg_tr", overwrite = TRUE)
  hpc_linreg_te <- copy_to(sc, hpc[  1:4 , -1], "hpc_linreg_te", overwrite = TRUE)

  expect_error(
    spark_fit <-
      fit(
        linear_reg() %>% set_engine("spark"),
        control = ctrl,
        Sepal_Length ~ .,
        data = hpc_linreg_tr
      ),
    regexp = NA
  )

  expect_false(has_multi_predict(spark_fit))
  expect_equal(multi_predict_args(spark_fit), NA_character_)

  expect_error(
    spark_pred <- predict(spark_fit, hpc_linreg_te),
    regexp = NA
  )

  expect_error(
    spark_pred_num <- predict(spark_fit, hpc_linreg_te),
    regexp = NA
  )

  lm_fit <- lm(compounds ~ ., data = hpc[-(1:4),   ])
  lm_pred <- unname(predict(lm_fit, hpc[  1:4 , -1]))

  expect_equal(as.data.frame(spark_pred)$pred, lm_pred)
  expect_equal(as.data.frame(spark_pred_num)$pred, lm_pred)
})

