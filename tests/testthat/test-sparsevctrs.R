test_that("sparse tibble can be passed to `fit()", {
  skip_if_not_installed("xgboost")

  hotel_data <- sparse_hotel_rates()
  hotel_data <- sparsevctrs::coerce_to_sparse_tibble(hotel_data)

  spec <- boost_tree() %>%
    set_mode("regression") %>%
    set_engine("xgboost")

  expect_no_error(
    lm_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data)
  )

  spec <- linear_reg() %>%
    set_mode("regression") %>%
    set_engine("lm")

  expect_snapshot(
    lm_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data[1:100, ])
  )
})

test_that("sparse tibble can be passed to `fit_xy()", {
  skip_if_not_installed("xgboost")

  hotel_data <- sparse_hotel_rates()
  hotel_data <- sparsevctrs::coerce_to_sparse_tibble(hotel_data)

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
    lm_fit <- fit_xy(spec, x = hotel_data[1:100, -1], y = hotel_data[1:100, 1])
  )
})

test_that("sparse matrices can be passed to `fit_xy()", {
  skip_if_not_installed("xgboost")

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

test_that("sparse matrices can be passed to `predict()", {
  skip_if_not_installed("ranger")

  hotel_data <- sparse_hotel_rates()

  spec <- rand_forest(trees = 10) %>%
    set_mode("regression") %>%
    set_engine("ranger")

  tree_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])

  predict(tree_fit, hotel_data)
})


test_that("to_sparse_data_frame() is used correctly", {
  skip_if_not_installed("xgboost")
  
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
