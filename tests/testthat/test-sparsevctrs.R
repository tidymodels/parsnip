test_that("sparse matrices can be passed to `fit_xy()", {
  hotel_data <- sparse_hotel_rates()

  lm_spec <- linear_reg(penalty = 0) %>%
    set_engine("glmnet")

  expect_no_error(
    lm_fit <- fit_xy(lm_spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )
})

test_that("to_sparse_data_frame() is used correctly", {
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

  lm_spec <- linear_reg(penalty = 0) %>%
    set_engine("lm")

  expect_snapshot(
    error = TRUE,
    fit_xy(lm_spec, x = mtcars[, -1], y = mtcars[, 1])
  )
  expect_snapshot(
    error = TRUE,
    fit_xy(lm_spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )
  
  lm_spec <- linear_reg(penalty = 0) %>%
    set_engine("glmnet")

  expect_snapshot(
    error = TRUE,
    fit_xy(lm_spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )
})

test_that("maybe_sparse_matrix() is used correctly", {
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

  lm_spec <- linear_reg(penalty = 0) %>%
    set_engine("glmnet")

  expect_snapshot(
    error = TRUE,
    fit_xy(lm_spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )
  expect_snapshot(
    error = TRUE,
    fit_xy(lm_spec, x = mtcars[, -1], y = mtcars[, 1])
  )
  expect_snapshot(
    error = TRUE,
    fit_xy(lm_spec, x = as.data.frame(mtcars)[, -1], y = as.data.frame(mtcars)[, 1])
  )
  expect_snapshot(
    error = TRUE,
    fit_xy(lm_spec, x = tibble::as_tibble(mtcars)[, -1], y = tibble::as_tibble(mtcars)[, 1])
  )
})
