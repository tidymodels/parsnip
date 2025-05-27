skip_if_not_installed("modeldata")

lending_club <- head(lending_club, 200)
lending_club_fail <-
  lending_club |>
  dplyr::mutate(bad = Inf, miss = NA)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
lc_basic <-
  boost_tree(mode = "classification")  |>
      set_engine("C5.0", bands = 2)

# ------------------------------------------------------------------------------

test_that('C5.0 execution', {

  skip_if_not_installed("C50")

  expect_no_condition(
    res <- fit(
      lc_basic,
      Class ~ log(funded_amnt) + int_rate,
      data = lending_club,
      control = ctrl
    )
  )

  expect_no_condition(
    res <- fit_xy(
      lc_basic,
      x = lending_club[, num_pred],
      y = lending_club$Class,
      control = ctrl
    )
  )

  expect_true(has_multi_predict(res))
  expect_equal(multi_predict_args(res), "trees")

  # outcome is not a factor:
  expect_snapshot(
    error = TRUE,
    res <- fit(
      lc_basic,
      funded_amnt ~ term,
      data = lending_club,
      engine = "C5.0",
      control = ctrl
    )
  )

  # Model fails
  C5.0_form_catch <- fit(
    lc_basic,
    Class ~ miss,
    data = lending_club_fail,
    control = caught_ctrl
  )
  expect_true(inherits(extract_fit_engine(C5.0_form_catch), "try-error"))

  # Model fails
  C5.0_xy_catch <- fit_xy(
    lc_basic,
    control = caught_ctrl,
    x = lending_club_fail[, "miss"],
    y = lending_club_fail$Class
  )
  expect_true(inherits(extract_fit_engine(C5.0_xy_catch), "try-error"))
})

test_that('C5.0 prediction', {

  skip_if_not_installed("C50")

  classes_xy <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- predict(extract_fit_engine(classes_xy), newdata = lending_club[1:7, num_pred])
  expect_equal(xy_pred, predict(classes_xy, lending_club[1:7, num_pred])$.pred_class)

})

test_that('C5.0 probabilities', {

  skip_if_not_installed("C50")

  classes_xy <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- predict(extract_fit_engine(classes_xy), newdata = as.data.frame(lending_club[1:7, num_pred]), type = "prob")
  xy_pred <- as_tibble(xy_pred)
  names(xy_pred) <- c(".pred_bad", ".pred_good")
  expect_equal(xy_pred, predict(classes_xy, lending_club[1:7, num_pred], type = "prob"))

  one_row <- predict(classes_xy, lending_club[1, num_pred], type = "prob")
  expect_equal(xy_pred[1,], one_row)

})


test_that('submodel prediction', {

  skip_if_not_installed("C50")
  library(C50)

  vars <- c("female", "tenure", "total_charges", "phone_service", "monthly_charges")
  class_fit <-
    boost_tree(trees = 20, mode = "classification") |>
    set_engine("C5.0", control = C5.0Control(earlyStopping = FALSE)) |>
    fit(churn ~ ., data = wa_churn[-(1:4), c("churn", vars)])

  pred_class <- predict(extract_fit_engine(class_fit), wa_churn[1:4, vars], trials = 4, type = "prob")

  mp_res <- multi_predict(class_fit, new_data = wa_churn[1:4, vars], trees = 4, type = "prob")
  mp_res <- do.call("rbind", mp_res$.pred)
  expect_equal(mp_res[[".pred_No"]], pred_class[, "No"], ignore_attr = TRUE)

  expect_snapshot(error = TRUE,
    multi_predict(class_fit, newdata = wa_churn[1:4, vars], trees = 4, type = "prob")
  )
})


## -----------------------------------------------------------------------------

test_that('argument checks for data dimensions', {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("C50")

  data(penguins, package = "modeldata")
  penguins <- na.omit(penguins)

  spec <-
    boost_tree(min_n = 1000, trees = 5) |>
    set_engine("C5.0") |>
    set_mode("classification")

  expect_snapshot(
    f_fit  <- spec |> fit(species ~ ., data = penguins)
  )
  expect_snapshot(
    xy_fit <- spec |> fit_xy(x = penguins[, -1], y = penguins$species)
  )

  expect_equal(extract_fit_engine(f_fit)$control$minCases,  nrow(penguins))
  expect_equal(extract_fit_engine(xy_fit)$control$minCases, nrow(penguins))

})
