library(dplyr)

# ------------------------------------------------------------------------------

context("adding functions")
source("helpers.R")

# ------------------------------------------------------------------------------

test_that('adding row indicies', {
  iris_2 <- iris %>% add_rowindex()
  expect_true(nrow(iris_2) == 150)
  expect_true(sum(names(iris_2) == ".row") == 1)
  expect_true(is.integer(iris_2$.row))

  mtcar_2 <- dplyr::as_tibble(mtcars) %>% dplyr::slice(0) %>% add_rowindex()
  expect_true(nrow(mtcar_2) == 0)
  expect_true(sum(names(mtcar_2) == ".row") == 1)
  expect_true(is.integer(mtcar_2$.row))

  expect_error(as.matrix(mtcars) %>% add_rowindex())
})
