skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('updating', {
  expect_snapshot(
    boost_tree(trees = 1) |>
      set_engine("C5.0", noGlobalPruning = TRUE) |>
      update(trees = tune(), noGlobalPruning = tune())
  )
})

test_that('bad input', {
  expect_snapshot(error = TRUE, boost_tree(mode = "bogus"))
  expect_snapshot(translate(boost_tree(mode = "classification"), engine = NULL))
})


## -----------------------------------------------------------------------------

test_that('argument checks for data dimensions', {
  skip_if_not_installed("sparklyr")
  library(sparklyr)
  skip_if(nrow(spark_installed_versions()) == 0)

  spec <-
    boost_tree(mtry = 1000, min_n = 1000, trees = 5) |>
    set_engine("spark") |>
    set_mode("classification")

  args <- translate(spec)$method$fit$args
  expect_equal(args$min_instances_per_node, expr(min_rows(1000, x)))

  sc = spark_connect(master = "local")
  cars = copy_to(sc, mtcars, overwrite = TRUE)
  expect_equal(min_rows(10, cars), 10)
})

test_that('boost_tree can be fit with 1 predictor if validation is used', {
  skip_if_not_installed("earth")
  skip_on_cran()
  spec <- boost_tree(trees = 1) |>
    set_engine("xgboost", validation = 0.5) |>
    set_mode("regression")

  expect_no_error(
    fit(spec, mpg ~ disp, data = mtcars)
  )
})

test_that("check_args() works", {
  skip_if_not_installed("xgboost")
  skip_on_cran()

  expect_snapshot(
    error = TRUE,
    {
      spec <- boost_tree(trees = -1) |>
        set_engine("xgboost") |>
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      spec <- boost_tree(sample_size = -10) |>
        set_engine("xgboost") |>
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      spec <- boost_tree(tree_depth = -10) |>
        set_engine("xgboost") |>
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      spec <- boost_tree(min_n = -10) |>
        set_engine("xgboost") |>
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
})
