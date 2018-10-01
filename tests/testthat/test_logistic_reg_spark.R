library(testthat)
context("logistic regression execution with spark")
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

  churn_logit_tr <- copy_to(sc, wa_churn[ 5:100,   ], "churn_logit_tr", overwrite = TRUE)
  churn_logit_te <- copy_to(sc, wa_churn[   1:4, -1], "churn_logit_te", overwrite = TRUE)

  # ----------------------------------------------------------------------------

  expect_error(
    spark_class_fit <-
      fit(
        logistic_reg(),
        engine = "spark",
        control = ctrl,
        churn ~ .,
        data = churn_logit_tr
      ),
    regexp = NA
  )

  # check for reproducibility and passing extra arguments
  expect_error(
    spark_class_fit_dup <-
      fit(
        logistic_reg(),
        engine = "spark",
        control = ctrl,
        churn ~ .,
        data = churn_logit_tr
      ),
    regexp = NA
  )


})

