library(dplyr)

# ------------------------------------------------------------------------------

source("helpers.R")
source(test_path("helper-objects.R"))
hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('adding row indicies', {
  hpc_2 <- hpc %>% add_rowindex()
  expect_true(nrow(hpc_2) == 150)
  expect_true(sum(names(hpc_2) == ".row") == 1)
  expect_true(is.integer(hpc_2$.row))

  mtcar_2 <- dplyr::as_tibble(mtcars) %>% dplyr::slice(0) %>% add_rowindex()
  expect_true(nrow(mtcar_2) == 0)
  expect_true(sum(names(mtcar_2) == ".row") == 1)
  expect_true(is.integer(mtcar_2$.row))

  expect_error(as.matrix(mtcars) %>% add_rowindex())
})
