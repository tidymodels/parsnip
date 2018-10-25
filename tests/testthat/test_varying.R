library(testthat)
library(parsnip)
library(rlang)
library(dplyr)

context("varying parameters")

load("recipes_examples.RData")

test_that('main parsnip arguments', {

  mod_1 <-
    rand_forest() %>%
    varying_args(id = "")
  exp_1 <-
    tibble(
      name = c("mtry", "trees", "min_n"),
      varying = rep(FALSE, 3),
      id = rep("", 3),
      type = rep("model_spec", 3)
    )
  expect_equal(mod_1, exp_1)

  mod_2 <-
    rand_forest(mtry = varying()) %>%
    varying_args(id = "")
  exp_2 <- exp_1
  exp_2$varying[1] <- TRUE
  expect_equal(mod_2, exp_2)

  mod_3 <-
    rand_forest(mtry = varying(), trees  = varying()) %>%
    varying_args(id = "wat")
  exp_3 <- exp_2
  exp_3$varying[1:2] <- TRUE
  exp_3$id <- "wat"
  expect_equal(mod_3, exp_3)
})


test_that('other parsnip arguments', {

  other_1 <-
    rand_forest() %>%
    set_engine("ranger", sample.fraction = varying()) %>%
    varying_args(id = "only engine args")
  exp_1 <-
    tibble(
      name = c("mtry", "trees", "min_n", "sample.fraction"),
      varying = c(rep(FALSE, 3), TRUE),
      id = rep("only engine args", 4),
      type = rep("model_spec", 4)
    )
  expect_equal(other_1, exp_1)

  other_2 <-
    rand_forest(min_n = varying())  %>%
    set_engine("ranger", sample.fraction = varying()) %>%
    varying_args(id = "only engine args")
  exp_2 <-
    tibble(
      name = c("mtry", "trees", "min_n", "sample.fraction"),
      varying = c(rep(FALSE, 2), rep(TRUE, 2)),
      id = rep("only engine args", 4),
      type = rep("model_spec", 4)
    )
  expect_equal(other_2, exp_2)

  other_3 <-
    rand_forest()  %>%
    set_engine("ranger", strata = Class, sampsize = c(varying(), varying())) %>%
    varying_args(id = "add an expr")
  exp_3 <-
    tibble(
      name = c("mtry", "trees", "min_n", "strata", "sampsize"),
      varying = c(rep(FALSE, 4), TRUE),
      id = rep("add an expr", 5),
      type = rep("model_spec", 5)
    )
  expect_equal(other_3, exp_3)

  other_4 <-
    rand_forest()  %>%
    set_engine("ranger", strata = Class, sampsize = c(12, varying())) %>%
    varying_args(id = "num and varying in vec")
  exp_4 <-
    tibble(
      name = c("mtry", "trees", "min_n", "strata", "sampsize"),
      varying = c(rep(FALSE, 4), TRUE),
      id = rep("num and varying in vec", 5),
      type = rep("model_spec", 5)
    )
  expect_equal(other_4, exp_4)
})


test_that('recipe parameters', {

  rec_res_1 <- varying_args(rec_1)
  exp_1 <-
    tibble(
      name = c("K", "num", "threshold", "options"),
      varying = c(TRUE, TRUE, FALSE, FALSE),
      id = c("step_knnimpute", rep("step_pca", 3)),
      type = rep("step", 4)
    )
  expect_equal(rec_res_1, exp_1)

  rec_res_2 <- varying_args(rec_2)
  exp_2 <- exp_1
  expect_equal(rec_res_2, exp_2)

  rec_res_3 <- varying_args(rec_3)
  exp_3 <- exp_1
  exp_3$varying <- FALSE
  expect_equal(rec_res_3, exp_3)

  rec_res_4 <- varying_args(rec_4)
  exp_4 <- tibble()
  expect_equal(rec_res_4, exp_4)
})

