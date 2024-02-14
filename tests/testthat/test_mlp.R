
test_that('updating', {
  expect_snapshot(
    mlp(mode = "classification", hidden_units = 2) %>%
      set_engine("nnet", Hess = FALSE) %>%
      update(hidden_units = tune(), Hess = tune())
  )
})

test_that('bad input', {
  expect_error(mlp(mode = "time series"))
  expect_error(translate(mlp(mode = "classification") %>% set_engine("wat?")))
  expect_warning(translate(mlp(mode = "regression") %>% set_engine("nnet", formula = y ~ x)))
  expect_error(translate(mlp(mode = "classification", x = x, y = y) %>% set_engine("keras")))
  expect_error(translate(mlp(mode = "regression", formula = y ~ x) %>% set_engine()))
})

test_that("nnet_softmax", {
  obj <- mlp(mode = 'classification')
  obj$lvls <- c("a", "b")
  res <- nnet_softmax(matrix(c(.8, .2)), obj)
  expect_equal(names(res), obj$lvls)
  expect_equal(res$b, 1 - res$a)
})

test_that("more activations for brulee", {
  skip_if_not_installed("brulee")
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
      mlp(penalty = 0.10, activation = "softplus") %>%
        set_mode("regression") %>%
        set_engine("brulee") %>%
        fit_xy(x = as.matrix(ames_train[, c("Longitude", "Latitude")]),
               y = ames_train$Sale_Price),
      silent = TRUE)
  expect_true(inherits(fit$fit, "brulee_mlp"))
})

