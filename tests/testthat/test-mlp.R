skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

test_that('updating', {
  expect_snapshot(
    mlp(mode = "classification", hidden_units = 2) |>
      set_engine("nnet", Hess = FALSE) |>
      update(hidden_units = tune(), Hess = tune())
  )
})

test_that('bad input', {
  expect_snapshot(error = TRUE, mlp(mode = "time series"))
  expect_snapshot(error = TRUE, translate(mlp(mode = "classification") |> set_engine("wat?")))
  expect_warning(
    translate(mlp(mode = "regression") |> set_engine("nnet", formula = y ~ x)),
    class = "parsnip_protected_arg_warning"
  )
})

test_that("nnet_softmax", {
  obj <- mlp(mode = 'classification')
  obj$lvls <- c("a", "b")
  res <- nnet_softmax(matrix(c(.8, .2)), obj)
  expect_equal(names(res), obj$lvls)
  expect_equal(res$b, 1 - res$a)
})

test_that("more activations for brulee", {
  skip_if_not_installed("brulee", minimum_version = "0.3.0")
  skip_if_not_installed("modeldata")
  skip_on_cran()

  data(ames, package = "modeldata")

  ames$Sale_Price <- log10(ames$Sale_Price)

  set.seed(122)
  in_train <- sample(1:nrow(ames), 2000)
  ames_train <- ames[ in_train,]
  ames_test  <- ames[-in_train,]

  set.seed(1)
  fit <-
    try(
      mlp(penalty = 0.10, activation = "softplus") |>
        set_mode("regression") |>
        set_engine("brulee") |>
        fit_xy(x = as.matrix(ames_train[, c("Longitude", "Latitude")]),
               y = ames_train$Sale_Price),
      silent = TRUE)
  expect_true(inherits(fit$fit, "brulee_mlp"))
})

test_that("check_args() works", {
  skip_if_not_installed("keras")

  expect_snapshot(
    error = TRUE,
    {
      spec <- mlp(penalty = -1) |>
        set_engine("keras") |>
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      spec <- mlp(dropout = -1) |>
        set_engine("keras") |>
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      spec <- mlp(dropout = 1, penalty = 3) |>
        set_engine("keras") |>
        set_mode("classification")
      fit(spec, class ~ ., hpc)
    }
  )
})

# ------------------------------------------------------------------------------

test_that("tunables", {

  expect_snapshot(
    mlp() |>
      set_engine("brulee") |>
      tunable()
  )
  expect_snapshot(
    mlp() |>
      set_engine("brulee_two_layer") |>
      tunable()
  )

  expect_snapshot(
    mlp() |>
      set_engine("nnet") |>
      tunable()
  )

  expect_snapshot(
    mlp() |>
      set_engine("keras") |>
      tunable()
  )

})
