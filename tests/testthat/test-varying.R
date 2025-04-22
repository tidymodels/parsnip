test_that('main parsnip arguments', {
  rlang::local_options(lifecycle_verbosity = "quiet")

  mod_1 <- rand_forest() |>
    varying_args()

  exp_1 <- tibble(
    name = c("mtry", "trees", "min_n"),
    varying = rep(FALSE, 3),
    id = rep("rand_forest", 3),
    type = rep("model_spec", 3)
  )

  expect_equal(mod_1, exp_1)

  mod_2 <- rand_forest(mtry = varying()) |>
    varying_args()

  exp_2 <- exp_1
  exp_2$varying[1] <- TRUE
  expect_equal(mod_2, exp_2)

  mod_3 <- rand_forest(mtry = varying(), trees  = varying()) |>
    varying_args()

  exp_3 <- exp_2
  exp_3$varying[1:2] <- TRUE
  expect_equal(mod_3, exp_3)
})


test_that('other parsnip arguments', {
  rlang::local_options(lifecycle_verbosity = "quiet")

  other_1 <- rand_forest() |>
    set_engine("ranger", sample.fraction = varying()) |>
    varying_args()

  exp_1 <- tibble(
    name = c("mtry", "trees", "min_n", "sample.fraction"),
    varying = c(rep(FALSE, 3), TRUE),
    id = rep("rand_forest", 4),
    type = rep("model_spec", 4)
  )

  expect_equal(other_1, exp_1)

  other_2 <- rand_forest(min_n = varying())  |>
    set_engine("ranger", sample.fraction = varying()) |>
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
  other_3 <- rand_forest()  |>
    set_engine("ranger", strata = Class, sampsize = c(varying(), varying())) |>
    varying_args()

  exp_3 <- tibble(
      name = c("mtry", "trees", "min_n", "strata", "sampsize"),
      varying = c(rep(FALSE, 4), TRUE),
      id = rep("rand_forest", 5),
      type = rep("model_spec", 5)
    )

  expect_equal(other_3, exp_3)

  other_4 <- rand_forest()  |>
    set_engine("ranger", strata = Class, sampsize = c(12, varying())) |>
    varying_args()

  exp_4 <- tibble(
    name = c("mtry", "trees", "min_n", "strata", "sampsize"),
    varying = c(rep(FALSE, 4), TRUE),
    id = rep("rand_forest", 5),
    type = rep("model_spec", 5)
  )

  expect_equal(other_4, exp_4)
})

test_that("empty lists return FALSE - #131", {
  expect_equal(
    parsnip:::find_varying(list()),
    FALSE
  )
})

test_that("lists with multiple elements return a single logical - #131", {
  rlang::local_options(lifecycle_verbosity = "quiet")
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
  rlang::local_options(lifecycle_verbosity = "quiet")
  deep_varying <- rlang::call2("list", x = list(xx = list(xxx = varying())))

  expect_equal(
    parsnip:::find_varying(deep_varying),
    TRUE
  )
})
