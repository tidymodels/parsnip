#' Augment data with predictions
#'
#' `augment()` will add column(s) for predictions to the given data.
#'
#' @param x A [model fit][model_fit] produced by [fit.model_spec()] or
#' [fit_xy.model_spec()].
#' @param eval_time For censored regression models, a vector of time points at
#' which the survival probability is estimated.
#' @details
#'
#' ## Regression
#' For regression models, a `.pred` column is added. If `x` was created using
#' [fit.model_spec()] and `new_data` contains a regression outcome column, a
#' `.resid` column is also added.
#'
#' ## Classification
#'
#' For classification models, the results can include a column called
#'  `.pred_class` as well as class probability columns named `.pred_{level}`.
#'  This depends on what type of prediction types are available for the model.
#'
#' ## Censored Regression
#'
#' For these models, predictions for the expected time and survival probability
#' are created (if the model engine supports them). If the model supports
#' survival prediction, the `eval_time` argument is required.
#'
#' If survival predictions are created and `new_data` contains a
#' [survival::Surv()] object, additional columns are added for inverse
#' probability of censoring weights (IPCW) are also created (see `tidymodels.org`
#' page in the references below). This enables the user to compute performance
#' metrics in the \pkg{yardstick} package.
#'
#' @param new_data A data frame or matrix.
#' @param ... Not currently used.
#' @rdname augment
#' @references
#' \url{https://www.tidymodels.org/learn/statistics/survival-metrics/}
#' @export
#' @examplesIf !parsnip:::is_cran_check() & rlang::is_installed("modeldata")
#' car_trn <- mtcars[11:32,]
#' car_tst <- mtcars[ 1:10,]
#'
#' reg_form <-
#'   linear_reg() |>
#'   set_engine("lm") |>
#'   fit(mpg ~ ., data = car_trn)
#' reg_xy <-
#'   linear_reg() |>
#'   set_engine("lm") |>
#'   fit_xy(car_trn[, -1], car_trn$mpg)
#'
#' augment(reg_form, car_tst)
#' augment(reg_form, car_tst[, -1])
#'
#' augment(reg_xy, car_tst)
#' augment(reg_xy, car_tst[, -1])
#'
#' # ------------------------------------------------------------------------------
#'
#' data(two_class_dat, package = "modeldata")
#' cls_trn <- two_class_dat[-(1:10), ]
#' cls_tst <- two_class_dat[  1:10 , ]
#'
#' cls_form <-
#'   logistic_reg() |>
#'   set_engine("glm") |>
#'   fit(Class ~ ., data = cls_trn)
#' cls_xy <-
#'   logistic_reg() |>
#'   set_engine("glm") |>
#'   fit_xy(cls_trn[, -3],
#'   cls_trn$Class)
#'
#' augment(cls_form, cls_tst)
#' augment(cls_form, cls_tst[, -3])
#'
#' augment(cls_xy, cls_tst)
#' augment(cls_xy, cls_tst[, -3])
#'
augment.model_fit <- function(x, new_data, eval_time = NULL, ...) {
  new_data <- tibble::new_tibble(new_data)
  res <-
    switch(
      x$spec$mode,
      "regression"          = augment_regression(x, new_data),
      "classification"      = augment_classification(x, new_data),
      "censored regression" = augment_censored(x, new_data, eval_time = eval_time),
      cli::cli_abort(
        c(
          "Unknown mode {.val {x$spec$mode}}.",
          "i" = "Model mode should be one of {.or {.val {all_modes}}}."
        )
      )
    )
  tibble::new_tibble(res)
}

augment_regression <- function(x, new_data) {
  ret <- new_data
  check_spec_pred_type(x, "numeric")
  ret <- dplyr::bind_cols(predict(x, new_data = new_data), ret)
  if (length(x$preproc$y_var) > 0) {
    y_nm <- x$preproc$y_var
    if (any(names(new_data) == y_nm)) {
      ret <- dplyr::mutate(ret, .resid = !!rlang::sym(y_nm) - .pred)
    }
  }
  dplyr::relocate(ret, dplyr::starts_with(".pred"), dplyr::starts_with(".resid"))
}

augment_classification <- function(x, new_data) {
  ret <- new_data

  if (spec_has_pred_type(x, "prob")) {
    ret <- dplyr::bind_cols(predict(x, new_data = new_data, type = "prob"), ret)
  }

  if (spec_has_pred_type(x, "class")) {
    ret <- dplyr::bind_cols(predict(x, new_data = new_data, type = "class"), ret)
  }
  ret
}

# nocov start
# tested in tidymodels/extratests#
augment_censored <- function(x, new_data, eval_time = NULL) {
  ret <- new_data

  if (spec_has_pred_type(x, "time")) {
    ret <- dplyr::bind_cols(predict(x, new_data = new_data, type = "time"), ret)
  }

  if (spec_has_pred_type(x, "survival")) {
    if (is.null(eval_time)) {
      cli::cli_abort(
        c(
          x = "The {.arg eval_time} argument is missing, with no default.",
          i = "{.arg eval_time} is required to be able to calculate \\
              predictions of survival probability."
        ),
        call = caller_env()
      )
    }
    .filter_eval_time(eval_time)
    ret <- dplyr::bind_cols(
      predict(x, new_data = new_data, type = "survival", eval_time = eval_time),
      ret)
    # Add inverse probability weights when the outcome is present in new_data
    y_col <- .find_surv_col(new_data, fail = FALSE)
    if (length(y_col) != 0) {
      ret <- .censoring_weights_graf(x, ret)
    }
  }
  ret
}
# nocov end
