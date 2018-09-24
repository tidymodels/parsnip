library(testthat)
context("multinomial regression execution with spark")
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

  iris_rows <- c(1, 51, 101)
  iris_tr <- copy_to(sc, iris[-iris_rows,   ], "iris_tr", overwrite = TRUE)
  iris_te <- copy_to(sc, iris[ iris_rows, -5], "iris_te", overwrite = TRUE)

  # ----------------------------------------------------------------------------

  expect_error(
    spark_class_fit <-
      fit(
        multinom_reg(),
        engine = "spark",
        control = ctrl,
        Species ~ .,
        data = iris_tr
      ),
    regexp = NA
  )

  expect_error(
    spark_class_pred <- predict(spark_class_fit, iris_te),
    regexp = NA
  )

  expect_error(
    spark_class_pred_class <- predict_class(spark_class_fit, iris_te),
    regexp = NA
  )

  expect_equal(colnames(spark_class_pred), "pred_class")

  expect_equal(
    as.data.frame(spark_class_pred)$pred_class,
    as.data.frame(spark_class_pred_class)$pred_class
  )

  expect_error(
    spark_class_prob <- predict(spark_class_fit, iris_te, type = "prob"),
    regexp = NA
  )

  expect_error(
    spark_class_prob_classprob <- predict_classprob(spark_class_fit, iris_te),
    regexp = NA
  )

  expect_equal(
    colnames(spark_class_prob),
    c("pred_versicolor", "pred_virginica", "pred_setosa")
  )

  expect_equivalent(
    as.data.frame(spark_class_prob),
    as.data.frame(spark_class_prob_classprob)
  )
})

