library(testthat)
library(parsnip)
library(dplyr)

# ------------------------------------------------------------------------------

context("random forest execution with spark")

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
        rand_forest(trees = 5, mode = "regression") %>%
          set_engine("spark", seed = 12),
        control = ctrl,
        Sepal_Length ~ .,
        data = iris_rf_tr
      ),
    regexp = NA
  )

  # check for reproducibility and passing extra arguments
  expect_error(
    spark_reg_fit_dup <-
      fit(
        rand_forest(trees = 5, mode = "regression") %>%
          set_engine("spark", seed = 12),
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
    spark_reg_pred_num <- predict_numeric(spark_reg_fit, iris_rf_te),
    regexp = NA
  )

  expect_error(
    spark_reg_dup <- predict(spark_reg_fit_dup, iris_rf_te),
    regexp = NA
  )

  expect_error(
    spark_reg_num_dup <- predict_numeric(spark_reg_fit_dup, iris_rf_te),
    regexp = NA
  )

  expect_equal(colnames(spark_reg_pred), "pred")

  expect_equal(
    as.data.frame(spark_reg_pred)$pred,
    as.data.frame(spark_reg_dup)$pred
  )
  expect_equal(
    as.data.frame(spark_reg_pred_num)$pred,
    as.data.frame(spark_reg_num_dup)$pred
  )


  # ----------------------------------------------------------------------------

  # same for classification

  churn_rf_tr <- copy_to(sc, wa_churn[ 5:100,   ], "churn_rf_tr", overwrite = TRUE)
  churn_rf_te <- copy_to(sc, wa_churn[   1:4, -1], "churn_rf_te", overwrite = TRUE)

  # ----------------------------------------------------------------------------

  expect_error(
    spark_class_fit <-
      fit(
        rand_forest(trees = 5, mode = "classification") %>%
          set_engine("spark", seed = 12),
        control = ctrl,
        churn ~ .,
        data = churn_rf_tr
      ),
    regexp = NA
  )

  # check for reproducibility and passing extra arguments
  expect_error(
    spark_class_fit_dup <-
      fit(
        rand_forest(trees = 5, mode = "classification") %>%
          set_engine("spark", seed = 12),
        control = ctrl,
        churn ~ .,
        data = churn_rf_tr
      ),
    regexp = NA
  )

  expect_error(
    spark_class_pred <- predict(spark_class_fit, churn_rf_te),
    regexp = NA
  )

  expect_error(
    spark_class_pred_class <- predict_class(spark_class_fit, churn_rf_te),
    regexp = NA
  )

  expect_error(
    spark_class_dup <- predict(spark_class_fit_dup, churn_rf_te),
    regexp = NA
  )

  expect_error(
    spark_class_dup_class <- predict_class(spark_class_fit_dup, churn_rf_te),
    regexp = NA
  )

  expect_equal(colnames(spark_class_pred), "pred_class")

  expect_equal(
    as.data.frame(spark_class_pred)$pred_class,
    as.data.frame(spark_class_dup)$pred_class
  )
  expect_equal(
    as.data.frame(spark_class_pred_class)$pred_class,
    as.data.frame(spark_class_dup_class)$pred_class
  )


  expect_error(
    spark_class_prob <- predict(spark_class_fit, churn_rf_te, type = "prob"),
    regexp = NA
  )

  expect_error(
    spark_class_prob_classprob <- predict_classprob(spark_class_fit, churn_rf_te),
    regexp = NA
  )

  expect_error(
    spark_class_dup <- predict(spark_class_fit_dup, churn_rf_te, type = "prob"),
    regexp = NA
  )

  expect_error(
    spark_class_dup_classprob <- predict_classprob(spark_class_fit_dup, churn_rf_te),
    regexp = NA
  )

  expect_equal(colnames(spark_class_prob), c("pred_No", "pred_Yes"))

  expect_equivalent(
    as.data.frame(spark_class_prob),
    as.data.frame(spark_class_dup)
  )
  expect_equal(
    as.data.frame(spark_class_prob_classprob),
    as.data.frame(spark_class_dup_classprob)
  )

})

