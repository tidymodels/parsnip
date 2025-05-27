skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------

test_that('updating', {
  expect_snapshot(
    logistic_reg(mixture = 0) |>
      set_engine("glmnet", nlambda = 10) |>
      update(mixture = tune(), nlambda = tune())
  )
})

test_that('bad input', {
  expect_snapshot(error = TRUE, logistic_reg(mode = "regression"))
  expect_snapshot(error = TRUE, translate(logistic_reg(mixture = 0.5) |> set_engine(engine = "LiblineaR")))

  expect_snapshot(
    res <-
      mtcars |>
      dplyr::mutate(cyl = as.factor(cyl)) |>
      fit(logistic_reg(), cyl ~ mpg, data = .),
    error = TRUE
  )
})

# ------------------------------------------------------------------------------

lending_club <- head(lending_club, 200)
lc_form <- as.formula(Class ~ log(funded_amnt) + int_rate)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
lc_basic <- logistic_reg() |> set_engine("glm")
ll_basic <- logistic_reg() |> set_engine("LiblineaR")

test_that('glm execution', {


  # passes interactively but not on R CMD check
  # expect_no_condition(
  #   res <- fit(
  #     lc_basic,
  #     lc_form,
  #     data = lending_club,
  #     control = ctrl,
  #     engine = "glm"
  #   )
  # )
  expect_no_condition(
    res <- fit_xy(
      lc_basic,
      x = lending_club[, num_pred],
      y = lending_club$Class,
      control = ctrl
    )
  )

  expect_snapshot(
    error = TRUE,
    res <- fit(
      lc_basic,
      funded_amnt ~ term,
      data = lending_club,
      control = ctrl
    )
  )

  # wrong outcome type
  expect_snapshot(
    error = TRUE,
    glm_form_catch <- fit(
      lc_basic,
      funded_amnt ~ term,
      data = lending_club,
      control = caught_ctrl
    )
  )

  expect_snapshot(
    error = TRUE,
    glm_xy_catch <- fit_xy(
      lc_basic,
      control = caught_ctrl,
      x = lending_club[, num_pred],
      y = lending_club$total_bal_il
    )
  )
})

test_that('glm prediction', {
  classes_xy <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- predict(extract_fit_engine(classes_xy), newdata = lending_club[1:7, num_pred], type = "response")
  xy_pred <- ifelse(xy_pred >= 0.5, "good", "bad")
  xy_pred <- factor(xy_pred, levels = levels(lending_club$Class))
  xy_pred <- unname(xy_pred)
  expect_equal(xy_pred, predict(classes_xy, lending_club[1:7, num_pred], type = "class")$.pred_class)

})

test_that('glm probabilities', {
  classes_xy <- fit_xy(
    lc_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- unname(predict(extract_fit_engine(classes_xy),
                            newdata = lending_club[1:7, num_pred],
                            type = "response"))
  xy_pred <- tibble(.pred_bad = 1 - xy_pred, .pred_good = xy_pred)
  expect_equal(xy_pred, predict(classes_xy, lending_club[1:7, num_pred], type = "prob"))

  one_row <- predict(classes_xy, lending_club[1, num_pred], type = "prob")
  expect_equal(xy_pred[1,], one_row)

})



test_that('glm intervals', {
  stats_glm <- glm(Class ~ log(funded_amnt) + int_rate, data = lending_club,
                   family = binomial)
  pred_glm <- predict(stats_glm, newdata = lending_club[1:5, ], se.fit = TRUE)
  t_val <- qt(0.035, df = stats_glm$df.residual, lower.tail = FALSE)
  lower_glm <- pred_glm$fit - t_val * pred_glm$se.fit
  upper_glm <- pred_glm$fit + t_val * pred_glm$se.fit

  lower_glm <- stats_glm$family$linkinv(lower_glm)
  upper_glm <- stats_glm$family$linkinv(upper_glm)

  res <- fit(
    logistic_reg() |> set_engine("glm"),
    Class ~ log(funded_amnt) + int_rate,
    data = lending_club,
    control = ctrl
  )

  confidence_parsnip <-
    predict(res,
            new_data = lending_club[1:5,],
            type = "conf_int",
            level = 0.93,
            std_error = TRUE)

  expect_equal(confidence_parsnip$.pred_lower_good, lower_glm)
  expect_equal(confidence_parsnip$.pred_upper_good, upper_glm)
  expect_equal(confidence_parsnip$.pred_lower_bad, 1 - upper_glm)
  expect_equal(confidence_parsnip$.pred_upper_bad, 1 - lower_glm)
  expect_equal(confidence_parsnip$.std_error, pred_glm$se.fit)

})

test_that('liblinear execution', {

  skip_if_not_installed("LiblineaR")

  expect_no_condition(
    res <- fit_xy(
      ll_basic,
      x = lending_club[, num_pred],
      y = lending_club$Class,
      control = ctrl
    )
  )

  expect_snapshot(
    error = TRUE,
    res <- fit(
      ll_basic,
      funded_amnt ~ term,
      data = lending_club,
      control = ctrl
    )
  )

  expect_no_condition(
    tidy_res <- tidy(res)
  )
  expect_s3_class(tidy_res, "tbl_df")
  expect_equal(colnames(tidy_res), c("term", "estimate"))

  # wrong outcome type
  expect_snapshot(
    error = TRUE,
    glm_form_catch <- fit(
      ll_basic,
      funded_amnt ~ term,
      data = lending_club,
      control = caught_ctrl
    )
  )

  expect_snapshot(
    error = TRUE,
    glm_xy_catch <- fit_xy(
      ll_basic,
      control = caught_ctrl,
      x = lending_club[, num_pred],
      y = lending_club$total_bal_il
    )
  )


})

test_that('liblinear prediction', {

  skip_if_not_installed("LiblineaR")

  classes_xy <- fit_xy(
    ll_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- predict(extract_fit_engine(classes_xy), newx = lending_club[1:7, num_pred])
  xy_pred <- xy_pred$predictions
  expect_equal(xy_pred, predict(classes_xy, lending_club[1:7, num_pred], type = "class")$.pred_class)

})

test_that('liblinear probabilities', {

  skip_if_not_installed("LiblineaR")

  classes_xy <- fit_xy(
    ll_basic,
    x = lending_club[, num_pred],
    y = lending_club$Class,
    control = ctrl
  )

  xy_pred <- predict(extract_fit_engine(classes_xy),
                     newx = lending_club[1:7, num_pred],
                     proba = TRUE)
  xy_pred <- as_tibble(xy_pred$probabilities)
  xy_pred <- tibble(.pred_good = xy_pred$good,
                    .pred_bad  = xy_pred$bad)
  expect_equal(xy_pred, predict(classes_xy, lending_club[1:7, num_pred], type = "prob"))

  one_row <- predict(classes_xy, lending_club[1, num_pred], type = "prob")
  expect_equal(xy_pred[1,], one_row)

})

test_that("check_args() works", {
  expect_snapshot(
    error = TRUE,
    {
      spec <- logistic_reg(mixture = -1) |>
        set_engine("glm") |>
        set_mode("classification")
      fit(spec, Class ~ ., lending_club)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      spec <- logistic_reg(penalty = -1) |>
        set_engine("glm") |>
        set_mode("classification")
      fit(spec, Class ~ ., lending_club)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      spec <- logistic_reg(mixture = 0.5) |>
        set_engine("LiblineaR") |>
        set_mode("classification")
      fit(spec, Class ~ ., lending_club)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      spec <- logistic_reg(penalty = 0) |>
        set_engine("LiblineaR") |>
        set_mode("classification")
      fit(spec, Class ~ ., lending_club)
    }
  )
})

# ------------------------------------------------------------------------------

test_that("tunables", {

  expect_snapshot(
    logistic_reg() |>
      tunable()
  )

  expect_snapshot(
    logistic_reg() |>
      set_engine("brulee") |>
      tunable()
  )
  expect_snapshot(
    logistic_reg() |>
      set_engine("glmnet") |>
      tunable()
  )

  expect_snapshot(
    logistic_reg() |>
      set_engine("keras") |>
      tunable()
  )

})
