library(testthat)
context("random forest execution with spark")
library(parsnip)
library(dplyr)

# ------------------------------------------------------------------------------

ctrl <- fit_control(verbosity = 1, catch = FALSE)
caught_ctrl <- fit_control(verbosity = 1, catch = TRUE)
quiet_ctrl <- fit_control(verbosity = 0, catch = TRUE)

# ------------------------------------------------------------------------------

test_that('spark execution', {

  skip_if_not_installed("sparklyr")

  library(sparklyr)

  sc <- try(spark_connect(master = "local"), silent = TRUE)

  skip_if(inherits(sc, "try-error"))

  iris_rf_tr <- copy_to(sc, iris[-(1:4),   ], "iris_rf_tr", overwrite = TRUE)
  iris_rf_te <- copy_to(sc, iris[  1:4 , -1], "iris_rf_te", overwrite = TRUE)

  # ----------------------------------------------------------------------------

  expect_error(
    spark_reg_fit <-
      fit(
        rand_forest(
          mode = "regression",
          others = list(seed = 12)
        ),
        engine = "spark",
        control = ctrl,
        Sepal_Length ~ .,
        data = iris_rf_tr
      ),
    regexp = NA
  )
  expect_error(
    spark_reg_fit_dup <-
      fit(
        rand_forest(
          mode = "regression",
          others = list(seed = 12)
        ),
        engine = "spark",
        control = ctrl,
        Sepal_Length ~ .,
        data = iris_rf_tr
      ),
    regexp = NA
  )

  expect_error(
    spark_reg_pred <- predict(spark_reg_fit, iris_rf_te),
    regexp = NA
  )

  expect_error(
    spark_reg_pred_num <- predict_num(spark_reg_fit, iris_rf_te),
    regexp = NA
  )

  expect_error(
    spark_reg_dup <- predict(spark_reg_fit_dup, iris_rf_te),
    regexp = NA
  )

  expect_error(
    spark_reg_num_dup <- predict_num(spark_reg_fit_dup, iris_rf_te),
    regexp = NA
  )


  expect_equal(
    as.data.frame(spark_reg_pred)$pred,
    as.data.frame(spark_reg_dup)$pred
  )
  expect_equal(
    as.data.frame(spark_reg_pred_num)$pred,
    as.data.frame(spark_reg_num_dup)$pred
  )

  # ----------------------------------------------------------------------------

})

