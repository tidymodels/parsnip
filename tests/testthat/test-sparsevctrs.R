skip_if_not_installed("modeldata")

test_that("sparse tibble can be passed to `fit() - supported", {
  skip_if_not_installed("xgboost")
  skip_on_cran()

  # Make materialization of sparse vectors throw an error
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  spec <- boost_tree() |>
    set_mode("regression") |>
    set_engine("xgboost")

  expect_snapshot(
    error = TRUE,
    xgb_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data)
  )
})

test_that("sparse tibble can be passed to `fit() - unsupported", {
  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  spec <- linear_reg() |>
    set_mode("regression") |>
    set_engine("lm")

  expect_snapshot(
    lm_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data[1:100, ])
  )
})

test_that("sparse matrix can be passed to `fit() - supported", {
  skip_if_not_installed("xgboost")
  skip_on_cran()

  # Make materialization of sparse vectors throw an error
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates()

  spec <- boost_tree() |>
    set_mode("regression") |>
    set_engine("xgboost")

  expect_snapshot(
    error = TRUE,
    xgb_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data)
  )

})

test_that("sparse matrix can be passed to `fit() - unsupported", {
  hotel_data <- sparse_hotel_rates()

  spec <- linear_reg() |>
    set_mode("regression") |>
    set_engine("lm")

  expect_snapshot(
    lm_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data[1:100, ])
  )
})

test_that("sparse tibble can be passed to `fit_xy() - supported", {
  skip_if_not_installed("xgboost")
  skip_on_cran()

  # Make materialization of sparse vectors throw an error
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  spec <- boost_tree() |>
    set_mode("regression") |>
    set_engine("xgboost")

  expect_no_error(
    xgb_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )
})

test_that("sparse tibble can be passed to `fit_xy() - unsupported", {
  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  spec <- linear_reg() |>
    set_mode("regression") |>
    set_engine("lm")

  expect_snapshot(
    lm_fit <- fit_xy(spec, x = hotel_data[1:100, -1], y = hotel_data[1:100, 1])
  )
})

test_that("sparse matrices can be passed to `fit_xy() - supported", {
  skip_if_not_installed("xgboost")
  skip_on_cran()

  # Make materialization of sparse vectors throw an error
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates()

  spec <- boost_tree() |>
    set_mode("regression") |>
    set_engine("xgboost")

  expect_no_error(
    xgb_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )
})

test_that("sparse matrices can be passed to `fit_xy() - unsupported", {
  hotel_data <- sparse_hotel_rates()

  spec <- linear_reg() |>
    set_mode("regression") |>
    set_engine("lm")

  expect_snapshot(
    lm_fit <- fit_xy(spec, x = hotel_data[1:100, -1], y = hotel_data[1:100, 1]),
    error = TRUE
  )
})

test_that("sparse tibble can be passed to `predict() - supported", {
  skip_if_not_installed("ranger")
  # Make materialization of sparse vectors throw an error
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  spec <- rand_forest(trees = 10) |>
    set_mode("regression") |>
    set_engine("ranger")

  tree_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])

  expect_no_error(
    predict(tree_fit, hotel_data)
  )
})

test_that("sparse tibble can be passed to `predict() - unsupported", {
  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  spec <- linear_reg() |>
    set_mode("regression") |>
    set_engine("lm")

  lm_fit <- fit(spec, mpg ~ ., data = mtcars)

  sparse_mtcars <- mtcars |>
    sparsevctrs::coerce_to_sparse_matrix() |>
    sparsevctrs::coerce_to_sparse_tibble()

  expect_snapshot(
    preds <- predict(lm_fit, sparse_mtcars)
  )
})

test_that("sparse matrices can be passed to `predict() - supported", {
  skip_if_not_installed("ranger")
  # Make materialization of sparse vectors throw an error
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates()

  spec <- rand_forest(trees = 10) |>
    set_mode("regression") |>
    set_engine("ranger")

  tree_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])

  expect_no_error(
    predict(tree_fit, hotel_data)
  )
})

test_that("sparse matrices can be passed to `predict() - unsupported", {
  hotel_data <- sparse_hotel_rates()

  spec <- linear_reg() |>
    set_mode("regression") |>
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
  skip_on_cran()

  # Make materialization of sparse vectors throw an error
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  spec <- boost_tree() |>
    set_mode("regression") |>
    set_engine("xgboost")

  hotel_data <- sparse_hotel_rates()

  expect_no_error(
    xgb_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
   )

   expect_no_error(
     predict(xgb_fit, hotel_data)
   )

  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  expect_snapshot(
    error = TRUE,
    xgb_fit <- fit(spec, avg_price_per_room ~ ., data = hotel_data)
  )

  expect_no_error(
    predict(xgb_fit, hotel_data)
  )

  expect_no_error(
    xgb_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )

  expect_no_error(
    predict(xgb_fit, hotel_data)
  )
})

test_that("to_sparse_data_frame() is used correctly", {
  skip_if_not_installed("xgboost")
  skip_on_cran()

  # Make materialization of sparse vectors throw an error
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
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

  spec <- linear_reg() |>
    set_engine("lm")

  expect_snapshot(
    error = TRUE,
    fit_xy(spec, x = mtcars[, -1], y = mtcars[, 1])
  )
  expect_snapshot(
    error = TRUE,
    fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )

  spec <- boost_tree() |>
    set_mode("regression") |>
    set_engine("xgboost")

  expect_snapshot(
    error = TRUE,
    fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])
  )
})

test_that("maybe_sparse_matrix() is used correctly", {
  skip_if_not_installed("xgboost")
  skip_on_cran()

  # Make materialization of sparse vectors throw an error
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  local_mocked_bindings(
    maybe_sparse_matrix = function(x) {
      if (sparsevctrs::has_sparse_elements(x)) {
        stop("sparse vectors detected")
      } else {
        stop("no sparse vectors detected")
      }
    }
  )

  hotel_data <- sparse_hotel_rates()

  spec <- boost_tree() |>
    set_mode("regression") |>
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

test_that("we don't run as.matrix() on sparse matrix for glmnet pred #1210", {
  skip_if_not_installed("glmnet")

  local_mocked_bindings(
    predict.elnet = function(object, newx, ...) {
      if (is_sparse_matrix(newx)) {
        stop("data is sparse")
      } else {
        stop("data isn't sparse (should not happen)")
      }
    },
    .package = "glmnet"
  )

  hotel_data <- sparse_hotel_rates()

  spec <- linear_reg(penalty = 0) |>
    set_mode("regression") |>
    set_engine("glmnet")

  lm_fit <- fit_xy(spec, x = hotel_data[, -1], y = hotel_data[, 1])

  expect_snapshot(
    error = TRUE,
    predict(lm_fit, hotel_data)
  )
})

test_that("fit() errors if sparse matrix has no colnames", {
  hotel_data <- sparse_hotel_rates()
  colnames(hotel_data) <- NULL

  spec <- boost_tree() |>
    set_mode("regression") |>
    set_engine("xgboost")

  expect_snapshot(
    error = TRUE,
    fit(spec, avg_price_per_room ~ ., data = hotel_data)
  )
})
