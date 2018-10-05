library(testthat)
context("linear regression execution with spark")
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

  iris_linreg_tr <- copy_to(sc, iris[-(1:4),   ], "iris_linreg_tr", overwrite = TRUE)
  iris_linreg_te <- copy_to(sc, iris[1:4 , -1], "iris_linreg_te", overwrite = TRUE)

  expect_error(
    spark_fit <-
      fit(
        linear_reg(),
        engine = "spark",
        control = ctrl,
        Sepal_Length ~ .,
        data = iris_linreg_tr
      ),
    regexp = NA
  )

  expect_silent(form_pred <- predict(spark_fit, iris_linreg_te))
  check_predict_basic(form_pred, iris[1:4 , -1])
})

