test_that("sparse tibble can be passed to `fit()", {
  skip_if_not_installed("xgboost")
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  spec <- boost_tree() %>%
    set_mode("regression") %>%
    set_engine("xgboost")
  
  expect_snapshot(
    error = TRUE,
    lm_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data)
  )

  spec <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine("lm")

  withr::local_options("sparsevctrs.verbose_materialize" = NULL)

  expect_snapshot(
    lm_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data[1:100, ])
  )
})

test_that("sparse matrix can be passed to `fit()", {
  skip_if_not_installed("xgboost")
  withr::local_options("sparsevctrs.verbose_materialize" = 3)
  
  hotel_data <- sparse_hotel_rates()
  
  spec <- boost_tree() %>%
    set_mode("regression") %>%
    set_engine("xgboost")

  expect_snapshot(
    error = TRUE,
    lm_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data)
  )

  withr::local_options("sparsevctrs.verbose_materialize" = NULL)

  spec <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine("lm")

  expect_snapshot(
    lm_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data[1:100, ])
  )
})

test_that("sparse tibble can be passed to `fit_xy()", {
  skip_if_not_installed("xgboost")
  
  hotel_data <- sparse_hotel_rates(tibble = TRUE)
  
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  spec <- boost_tree() %>%
    set_mode("regression") %>%
    set_engine("xgboost")

  expect_no_error(
    lm_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )

  withr::local_options("sparsevctrs.verbose_materialize" = NULL)

  spec <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine("lm")

  expect_snapshot(
    lm_fit <- fit_xy(spec, x = hotel_data[1:100, -1], y = hotel_data[1:100, 1])
  )
})

test_that("sparse matrices can be passed to `fit_xy()", {
  skip_if_not_installed("xgboost")
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates()

  spec <- boost_tree() %>%
    set_mode("regression") %>%
    set_engine("xgboost")

  expect_no_error(
    lm_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )

  spec <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine("lm")

  expect_snapshot(
    lm_fit <- fit_xy(spec, x = hotel_data[1:100, -1], y = hotel_data[1:100, 1]),
    error = TRUE
  )
})

test_that("sparse tibble can be passed to `predict()", {
  skip_if_not_installed("ranger")

  hotel_data <- sparse_hotel_rates(tibble = TRUE)
  
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  spec <- rand_forest(trees = 10) %>%
    set_mode("regression") %>%
    set_engine("ranger")

  tree_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])

  expect_no_error(
    predict(tree_fit, hotel_data)
  )

  withr::local_options("sparsevctrs.verbose_materialize" = NULL)

  spec <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine("lm")

  lm_fit <- fit(spec, mpg ~ ., data = mtcars)

  sparse_mtcars <- mtcars %>%
    sparsevctrs::coerce_to_sparse_matrix() %>%
    sparsevctrs::coerce_to_sparse_tibble()

  expect_snapshot(
    preds <- predict(lm_fit, sparse_mtcars)
  )
})

test_that("sparse matrices can be passed to `predict()", {
  skip_if_not_installed("ranger")
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates()

  spec <- rand_forest(trees = 10) %>%
    set_mode("regression") %>%
    set_engine("ranger")

  tree_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])

  expect_no_error(
    predict(tree_fit, hotel_data)
  )

  spec <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine("lm")

  lm_fit <- fit(spec, mpg ~ ., data = mtcars)

  sparse_mtcars <- sparsevctrs::coerce_to_sparse_matrix(mtcars)

  expect_snapshot(
    error = TRUE,
    predict(lm_fit, sparse_mtcars)
  )
})

test_that("sparse data work with xgboost engine", {
  skip_if_not_installed("xgboost")
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  spec <- boost_tree() %>%
    set_mode("regression") %>%
    set_engine("xgboost")

  hotel_data <- sparse_hotel_rates()

  expect_no_error(
    tree_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
   )
   
   expect_no_error(
     predict(tree_fit, hotel_data)
   )

  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  expect_snapshot(
    error = TRUE,
    tree_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data)
  )

  expect_no_error(
    predict(tree_fit, hotel_data)
  )
  
  expect_no_error(
   tree_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )

  expect_no_error(
    predict(tree_fit, hotel_data)
  )
})

test_that("to_sparse_data_frame() is used correctly", {
  skip_if_not_installed("xgboost")
  withr::local_options("sparsevctrs.verbose_materialize" = 3)
  
  local_mocked_bindings(
    to_sparse_data_frame = function(x, object) {
      if (methods::is(x, "sparseMatrix")) {
        if (allow_sparse(object)) {
          stop("x is spare, and sparse is allowed")
        } else {
          stop("x is spare, and sparse is not allowed")
        }
      }
      stop("x is not sparse")
    }
  )

  hotel_data <- sparse_hotel_rates()

  spec <- linear_reg() %>%
    set_engine("lm")

  expect_snapshot(
    error = TRUE,
    fit_xy(spec, x = mtcars[, -1], y = mtcars[, 1])
  )
  expect_snapshot(
    error = TRUE,
    fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )
  
  spec <- boost_tree() %>%
    set_mode("regression") %>%
    set_engine("xgboost")

  expect_snapshot(
    error = TRUE,
    fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )
})

test_that("maybe_sparse_matrix() is used correctly", {
  skip_if_not_installed("xgboost")
  withr::local_options("sparsevctrs.verbose_materialize" = 3)
  
  local_mocked_bindings(
    maybe_sparse_matrix = function(x) {
      if (is_sparse_tibble(x)) {
        stop("sparse vectors detected")
      } else {
        stop("no sparse vectors detected")
      }
    }
  )

  hotel_data <- sparse_hotel_rates()

  spec <- boost_tree() %>%
    set_mode("regression") %>%
    set_engine("xgboost")

  expect_snapshot(
    error = TRUE,
    fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )
  expect_snapshot(
    error = TRUE,
    fit_xy(spec, x = mtcars[, -1], y = mtcars[, 1])
  )
  expect_snapshot(
    error = TRUE,
    fit_xy(spec, x = as.data.frame(mtcars)[, -1], y = as.data.frame(mtcars)[, 1])
  )
  expect_snapshot(
    error = TRUE,
    fit_xy(spec, x = tibble::as_tibble(mtcars)[, -1], y = tibble::as_tibble(mtcars)[, 1])
  )
})
