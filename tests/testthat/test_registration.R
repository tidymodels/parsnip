library(parsnip)
library(dplyr)
library(rlang)
library(testthat)

# ------------------------------------------------------------------------------

context("model registration")
#source("helpers.R")

test_by_col <- function(a, b) {
  for(i in union(names(a), names(b))) {
    expect_equal(a[[i]], b[[i]])
  }
}

# ------------------------------------------------------------------------------

test_that('adding a new model', {
  set_new_model("sponge")

  mod_items <- get_model_env() %>% env_names()
  sponges <- grep("sponge", mod_items, value = TRUE)
  exp_obj <- c('sponge_modes', 'sponge_fit', 'sponge_args',
               'sponge_predict', 'sponge_pkgs', 'sponge')
  expect_equal(sort(sponges), sort(exp_obj))

  expect_equal(
    get_from_env("sponge"),
    tibble(engine = character(0), mode = character(0))
  )

test_by_col(
  get_from_env("sponge_pkgs"),
  tibble(engine = character(0), pkg = character(0))
)

expect_equal(
  get_from_env("sponge_modes"), "unknown"
)

test_by_col(
  get_from_env("sponge_args"),
  tibble(engine = character(0), parsnip = character(0),
         original = character(0), func = vector("list"))
)

test_by_col(
  get_from_env("sponge_fit"),
  tibble(engine = character(0), mode = character(0), value = vector("list"))
)

test_by_col(
  get_from_env("sponge_predict"),
  tibble(engine = character(0), mode = character(0),
         type = character(0), value = vector("list"))
)

expect_error(set_new_model())
# TODO expect_error(set_new_model(2))
# TODO expect_error(set_new_model(letters[1:2]))
})


# ------------------------------------------------------------------------------

test_that('adding a new mode', {
  set_model_mode("sponge", "classification")

  expect_equal(get_from_env("sponge_modes"), c("unknown", "classification"))

  # TODO expect_error(set_model_mode("sponge", "banana"))
  # TODO expect_error(set_model_mode("sponge", "classification"))

})


# ------------------------------------------------------------------------------

test_that('adding a new engine', {
  set_model_engine("sponge", "classification", "gum")

  test_by_col(
    get_from_env("sponge"),
    tibble(engine = "gum", mode = "classification")
  )


  expect_equal(get_from_env("sponge_modes"), c("unknown", "classification"))

  # TODO check for bad mode, check for duplicate

})


# ------------------------------------------------------------------------------

test_that('adding a new package', {
  set_dependency("sponge", "gum", "trident")

  expect_error(set_dependency("sponge", "gum", letters[1:2]))

  test_by_col(
    get_from_env("sponge_pkgs"),
    tibble(engine = "gum", pkg = list("trident"))
  )
})


# ------------------------------------------------------------------------------

test_that('adding a new argument', {

})



# ------------------------------------------------------------------------------

test_that('adding a new fit', {

})


# ------------------------------------------------------------------------------

test_that('adding a new predict method', {

})

