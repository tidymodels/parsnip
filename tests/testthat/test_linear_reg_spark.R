library(testthat)
library(parsnip)
library(dplyr)

# ------------------------------------------------------------------------------

context("linear regression execution with spark")
source("helper-objects.R")

# ------------------------------------------------------------------------------

test_that('spark execution', {

  skip_if_not_installed("sparklyr")

  library(sparklyr)

  sc <- try(spark_connect(master = "local"), silent = TRUE)

  skip_if(inherits(sc, "try-error"))

  iris_linreg_tr <- copy_to(sc, iris[-(1:4),   ], "iris_linreg_tr", overwrite = TRUE)
  iris_linreg_te <- copy_to(sc, iris[  1:4 , -1], "iris_linreg_te", overwrite = TRUE)

  expect_error(
    spark_fit <-
      fit(
        linear_reg() %>% set_engine("spark"),
        control = ctrl,
        Sepal_Length ~ .,
        data = iris_linreg_tr
      ),
    regexp = NA
  )

  expect_false(has_multi_predict(spark_fit))
  expect_equal(multi_predict_args(spark_fit), NA_character_)

  expect_error(
    spark_pred <- predict(spark_fit, iris_linreg_te),
    regexp = NA
  )

  expect_error(
    spark_pred_num <- predict(spark_fit, iris_linreg_te),
    regexp = NA
  )

  lm_fit <- lm(Sepal.Length ~ ., data = iris[-(1:4),   ])
  lm_pred <- unname(predict(lm_fit, iris[  1:4 , -1]))

  expect_equal(as.data.frame(spark_pred)$pred, lm_pred)
  expect_equal(as.data.frame(spark_pred_num)$pred, lm_pred)
})

