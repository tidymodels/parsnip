library(testthat)
context("boosted tree execution with spark")
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

  iris_bt_tr <- copy_to(sc, iris[-(1:4),   ], "iris_bt_tr", overwrite = TRUE)
  iris_bt_te <- copy_to(sc, iris[  1:4 , -1], "iris_bt_te", overwrite = TRUE)

  # ----------------------------------------------------------------------------

  expect_error(
    spark_reg_fit <-
      fit(
        boost_tree(
          trees = 5,
          mode = "regression",
          others = list(seed = 12)
        ),
        engine = "spark",
        control = ctrl,
        Sepal_Length ~ .,
        data = iris_bt_tr
      ),
    regexp = NA
  )

  expect_silent(form_pred <- predict(spark_reg_fit, iris_bt_te))
  check_predict_basic(form_pred, iris[  1:4 , -1])

  # check for reproducibility and passing extra arguments
  expect_error(
    spark_reg_fit_dup <-
      fit(
        boost_tree(
          trees = 5,
          mode = "regression",
          others = list(seed = 12)
        ),
        engine = "spark",
        control = ctrl,
        Sepal_Length ~ .,
        data = iris_bt_tr
      ),
    regexp = NA
  )

  # ----------------------------------------------------------------------------

  # same for classification

  churn_bt_tr <- copy_to(sc, wa_churn[ 5:100,   ], "churn_bt_tr", overwrite = TRUE)
  churn_bt_te <- copy_to(sc, wa_churn[   1:4, -1], "churn_bt_te", overwrite = TRUE)

  # ----------------------------------------------------------------------------

  expect_error(
    spark_class_fit <-
      fit(
        boost_tree(
          trees = 5,
          mode = "classification",
          others = list(seed = 12)
        ),
        engine = "spark",
        control = ctrl,
        churn ~ .,
        data = churn_bt_tr
      ),
    regexp = NA
  )

  # check for reproducibility and passing extra arguments
  expect_error(
    spark_class_fit_dup <-
      fit(
        boost_tree(
          trees = 5,
          mode = "classification",
          others = list(seed = 12)
        ),
        engine = "spark",
        control = ctrl,
        churn ~ .,
        data = churn_bt_tr
      ),
    regexp = NA
  )
})

