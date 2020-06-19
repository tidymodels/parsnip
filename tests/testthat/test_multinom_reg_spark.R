library(testthat)
library(parsnip)
library(dplyr)

# ------------------------------------------------------------------------------

context("multinomial regression execution with spark")
source(test_path("helper-objects.R"))
hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('spark execution', {

  skip_if_not_installed("sparklyr")

  library(sparklyr)

  sc <- try(spark_connect(master = "local"), silent = TRUE)

  skip_if(inherits(sc, "try-error"))

  hpc_rows <- c(1, 51, 101)
  hpc_tr <- copy_to(sc, hpc[-hpc_rows,   ], "hpc_tr", overwrite = TRUE)
  hpc_te <- copy_to(sc, hpc[ hpc_rows, -5], "hpc_te", overwrite = TRUE)

  # ----------------------------------------------------------------------------

  expect_error(
    spark_class_fit <-
      fit(
        multinom_reg() %>% set_engine("spark"),
        control = ctrl,
        class ~ .,
        data = hpc_tr
      ),
    regexp = NA
  )

  expect_error(
    spark_class_pred <- predict(spark_class_fit, hpc_te),
    regexp = NA
  )

  expect_error(
    spark_class_pred_class <- predict(spark_class_fit, hpc_te),
    regexp = NA
  )

  expect_equal(colnames(spark_class_pred), "pred_class")

  expect_equal(
    as.data.frame(spark_class_pred)$pred_class,
    as.data.frame(spark_class_pred_class)$pred_class
  )

  expect_error(
    spark_class_prob <- predict(spark_class_fit, hpc_te, type = "prob"),
    regexp = NA
  )

  expect_error(
    spark_class_prob_classprob <- predict(spark_class_fit, hpc_te, type = "prob"),
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

