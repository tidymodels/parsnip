library(testthat)
library(parsnip)
library(rlang)
library(dplyr)

context("varying parameters")

load(test_path("recipes_examples.RData"))

test_that('main parsnip arguments', {

  mod_1 <- rand_forest() %>%
    varying_args()

  exp_1 <- tibble(
    name = c("mtry", "trees", "min_n"),
    varying = rep(FALSE, 3),
    id = rep("rand_forest", 3),
    type = rep("model_spec", 3)
  )

  expect_equal(mod_1, exp_1)

  mod_2 <- rand_forest(mtry = varying()) %>%
    varying_args()

  exp_2 <- exp_1
  exp_2$varying[1] <- TRUE
  expect_equal(mod_2, exp_2)

  mod_3 <- rand_forest(mtry = varying(), trees  = varying()) %>%
    varying_args()

  exp_3 <- exp_2
  exp_3$varying[1:2] <- TRUE
  expect_equal(mod_3, exp_3)
})


test_that('other parsnip arguments', {

  other_1 <- rand_forest() %>%
    set_engine("ranger", sample.fraction = varying()) %>%
    varying_args()

  exp_1 <- tibble(
    name = c("mtry", "trees", "min_n", "sample.fraction"),
    varying = c(rep(FALSE, 3), TRUE),
    id = rep("rand_forest", 4),
    type = rep("model_spec", 4)
  )

  expect_equal(other_1, exp_1)

  other_2 <- rand_forest(min_n = varying())  %>%
    set_engine("ranger", sample.fraction = varying()) %>%
    varying_args()

  exp_2 <- tibble(
    name = c("mtry", "trees", "min_n", "sample.fraction"),
    varying = c(rep(FALSE, 2), rep(TRUE, 2)),
    id = rep("rand_forest", 4),
    type = rep("model_spec", 4)
  )

  expect_equal(other_2, exp_2)

  # We can detect these as varying, but they won't actually
  # be used in this way
  other_3 <- rand_forest()  %>%
    set_engine("ranger", strata = Class, sampsize = c(varying(), varying())) %>%
    varying_args()

  exp_3 <- tibble(
      name = c("mtry", "trees", "min_n", "strata", "sampsize"),
      varying = c(rep(FALSE, 4), TRUE),
      id = rep("rand_forest", 5),
      type = rep("model_spec", 5)
    )

  expect_equal(other_3, exp_3)

  other_4 <- rand_forest()  %>%
    set_engine("ranger", strata = Class, sampsize = c(12, varying())) %>%
    varying_args()

  exp_4 <- tibble(
    name = c("mtry", "trees", "min_n", "strata", "sampsize"),
    varying = c(rep(FALSE, 4), TRUE),
    id = rep("rand_forest", 5),
    type = rep("model_spec", 5)
  )

  expect_equal(other_4, exp_4)
})


test_that('recipe parameters', {

  # un-randomify the id names
  rec_1_id <- rec_1
  rec_1_id$steps[[1]]$id <- "center_1"
  rec_1_id$steps[[2]]$id <- "knnimpute_1"
  rec_1_id$steps[[3]]$id <- "pca_1"

  rec_res_1 <- varying_args(rec_1_id)

  exp_1 <- tibble(
    name = c("K", "num", "threshold", "options"),
    varying = c(TRUE, TRUE, FALSE, FALSE),
    id = c("knnimpute_1", rep("pca_1", 3)),
    type = rep("step", 4)
  )

  expect_equal(rec_res_1, exp_1)

  # un-randomify the id names
  rec_3_id <- rec_3
  rec_3_id$steps[[1]]$id <- "center_1"
  rec_3_id$steps[[2]]$id <- "knnimpute_1"
  rec_3_id$steps[[3]]$id <- "pca_1"

  rec_res_3 <- varying_args(rec_3_id)
  exp_3 <- exp_1
  exp_3$varying <- FALSE
  expect_equal(rec_res_3, exp_3)

  rec_res_4 <- varying_args(rec_4)

  exp_4 <- tibble(
    name = character(),
    varying = logical(),
    id = character(),
    type = character()
  )

  expect_equal(rec_res_4, exp_4)
})

test_that("empty lists return FALSE - #131", {
  expect_equal(
    parsnip:::find_varying(list()),
    FALSE
  )
})

test_that("lists with multiple elements return a single logical - #131", {
  expect_equal(
    parsnip:::find_varying(list(1, 2)),
    FALSE
  )

  expect_equal(
    parsnip:::find_varying(list(1, varying())),
    TRUE
  )
})

test_that("varying() deeply nested in calls can be located - #134", {
  deep_varying <- rlang::call2("list", x = list(xx = list(xxx = varying())))

  expect_equal(
    parsnip:::find_varying(deep_varying),
    TRUE
  )
})

test_that("recipe steps with non-varying args error if specified as varying()", {

  rec_bad_varying <- rec_1
  rec_bad_varying$steps[[1]]$skip <- varying()

  expect_error(
    varying_args(rec_bad_varying),
    "The following argument for a recipe step of type 'step_center' is not allowed to vary: 'skip'."
  )
})

test_that("`full = FALSE` returns only varying arguments", {

  x_spec <- rand_forest(min_n = varying())  %>%
    set_engine("ranger", sample.fraction = varying())

  x_rec <- rec_1

  expect_equal(
    varying_args(x_spec, full = FALSE)$name,
    c("min_n", "sample.fraction")
  )

  expect_equal(
    varying_args(x_rec, full = FALSE)$name,
    c("K", "num")
  )

})
