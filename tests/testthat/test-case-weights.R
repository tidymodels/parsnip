
test_that('case weights with xy method', {

  skip_if_not_installed("C50")
  skip_if_not_installed("modeldata")
  data("two_class_dat", package = "modeldata")

  wts <- runif(nrow(two_class_dat))
  wts <- ifelse(wts < 1/5, 0, 1)
  two_class_subset <- two_class_dat[wts != 0, ]
  wts <- importance_weights(wts)

  expect_no_condition({
    set.seed(1)
    C5_bst_wt_fit <-
      boost_tree(trees = 5) |>
      set_engine("C5.0") |>
      set_mode("classification") |>
      fit(Class ~ ., data = two_class_dat, case_weights = wts)
  })

  expect_output(
    print(C5_bst_wt_fit$fit$call),
    "weights = weights"
  )

  expect_no_condition({
    set.seed(1)
    C5_bst_wt_fit <-
      boost_tree(trees = 5) |>
      set_engine("C5.0") |>
      set_mode("classification") |>
      fit_xy(
        x = two_class_dat[c("A", "B")],
        y = two_class_dat$Class,
        case_weights = wts
      )
  })

  expect_output(
    print(C5_bst_wt_fit$fit$call),
    "weights = weights"
  )
})


test_that('case weights with xy method - non-standard argument names', {

  skip_if_not_installed("ranger")
  skip_if_not_installed("modeldata")
  data("two_class_dat", package = "modeldata")

  wts <- runif(nrow(two_class_dat))
  wts <- ifelse(wts < 1/5, 0, 1)
  two_class_subset <- two_class_dat[wts != 0, ]
  wts <- importance_weights(wts)

  expect_no_condition({
    set.seed(1)
    rf_wt_fit <-
      rand_forest(trees = 5) |>
      set_mode("classification") |>
      fit(Class ~ ., data = two_class_dat, case_weights = wts)
  })

  # expect_output(
  #   print(rf_wt_fit$fit$call),
  #   "case\\.weights = weights"
  # )

  expect_no_condition({
    set.seed(1)
    rf_wt_fit <-
      rand_forest(trees = 5) |>
      set_mode("classification") |>
      fit_xy(
        x = two_class_dat[c("A", "B")],
        y = two_class_dat$Class,
        case_weights = wts
      )
  })
})

test_that('case weights with formula method', {

  skip_if_not_installed("modeldata")
  data("ames", package = "modeldata")
  ames$Sale_Price <- log10(ames$Sale_Price)

  set.seed(1)
  wts <- runif(nrow(ames))
  wts <- ifelse(wts < 1/5, 0L, 1L)
  ames_subset <- ames[wts != 0, ]
  wts <- frequency_weights(wts)

  expect_no_condition(
    lm_wt_fit <-
      linear_reg() |>
      fit(Sale_Price ~ Longitude + Latitude, data = ames, case_weights = wts)
  )

  lm_sub_fit <-
    linear_reg() |>
    fit(Sale_Price ~ Longitude + Latitude, data = ames_subset)

  expect_equal(coef(lm_wt_fit$fit), coef(lm_sub_fit$fit))
})

test_that('case weights with formula method -- unregistered model spec', {

  skip_if_not_installed("modeldata")
  data("ames", package = "modeldata")
  ames$Sale_Price <- log10(ames$Sale_Price)

  set.seed(1)
  wts <- runif(nrow(ames))
  wts <- ifelse(wts < 1/5, 0L, 1L)
  ames_subset <- ames[wts != 0, ]
  wts <- frequency_weights(wts)

  expect_snapshot(
    error = TRUE,
    bag_mars("regression") |>
      fit(Sale_Price ~ Longitude + Latitude, data = ames, case_weights = wts)
  )
})

test_that('case weights with formula method that goes through `fit_xy()`', {

  skip_if_not_installed("modeldata")
  data("ames", package = "modeldata")
  ames$Sale_Price <- log10(ames$Sale_Price)

  set.seed(1)
  wts <- runif(nrow(ames))
  wts <- ifelse(wts < 1/5, 0L, 1L)
  ames_subset <- ames[wts != 0, ]
  wts <- frequency_weights(wts)

  expect_no_condition(
    lm_wt_fit <-
      linear_reg() |>
      fit_xy(
        x = ames[c("Longitude", "Latitude")],
        y = ames$Sale_Price,
        case_weights = wts
  ))

  lm_sub_fit <-
    linear_reg() |>
    fit_xy(
      x = ames_subset[c("Longitude", "Latitude")],
      y = ames_subset$Sale_Price
    )

  expect_equal(coef(lm_wt_fit$fit), coef(lm_sub_fit$fit))
})
