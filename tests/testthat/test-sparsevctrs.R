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

test_that("to_sparse_data_frame() is used correctly", {
  skip_if_not_installed("xgboost")
  
  skip_if_not_installed("LiblineaR")

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

  spec <- svm_linear() %>%
    set_mode("regression") %>%
    set_engine("xgboost")

  expect_snapshot(
    error = TRUE,
    fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )
})

test_that("maybe_sparse_matrix() is used correctly", {
  skip_if_not_installed("xgboost")
  
  skip_if_not_installed("LiblineaR")

  local_mocked_bindings(
    maybe_sparse_matrix = function(x) {
      if (any(vapply(x, sparsevctrs::is_sparse_vector, logical(1)))) {
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
