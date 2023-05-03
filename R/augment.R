#' Augment data with predictions
#'
#' `augment()` will add column(s) for predictions to the given data.
#'
#' For regression models, a `.pred` column is added. If `x` was created using
#' [fit.model_spec()] and `new_data` contains the outcome column, a `.resid` column is
#' also added.
#'
#' For classification models, the results can include a column called
#'  `.pred_class` as well as class probability columns named `.pred_{level}`.
#'  This depends on what type of prediction types are available for the model.
#' @param x A `model_fit` object produced by [fit.model_spec()] or
#' [fit_xy.model_spec()] .
#' @param new_data A data frame or matrix.
#' @param ... Not currently used.
#' @rdname augment
#' @export
#' @examples
#' car_trn <- mtcars[11:32,]
#' car_tst <- mtcars[ 1:10,]
#'
#' reg_form <-
#'   linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ ., data = car_trn)
#' reg_xy <-
#'   linear_reg() %>%
#'   set_engine("lm") %>%
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
#'   logistic_reg() %>%
#'   set_engine("glm") %>%
#'   fit(Class ~ ., data = cls_trn)
#' cls_xy <-
#'   logistic_reg() %>%
#'   set_engine("glm") %>%
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
  if (x$spec$mode == "regression") {
    res <- augment_regression(x, new_data)
  } else if (x$spec$mode == "classification") {
    res <- augment_classification(x, new_data)
  } else if (x$spec$mode == "censored regression") {
    res <- augment_classification(x, new_data)
  } else {
    rlang::abort(paste("Unknown mode:", x$spec$mode))
  }
  as_tibble(res)
}

augment_regression <- function(x, new_data) {
  check_spec_pred_type(x, "numeric")
  ret <-
    ret %>%
    dplyr::bind_cols(
      predict(x, new_data = new_data)
    )
  if (length(x$preproc$y_var) > 0) {
    y_nm <- x$preproc$y_var
    if (any(names(new_data) == y_nm)) {
      ret <- dplyr::mutate(ret, .resid = !!rlang::sym(y_nm) - .pred)
    }
  }
  dplyr::relocate(ret, dplyr::starts_with(".pred"))
}

augment_classification <- function(x, new_data) {
  if (spec_has_pred_type(x, "class")) {
    ret <- dplyr::bind_cols(
      ret,
      predict(x, new_data = new_data, type = "class")
    )
  }
  if (spec_has_pred_type(x, "prob")) {
    ret <- dplyr::bind_cols(
      ret,
      predict(x, new_data = new_data, type = "prob")
    )
  }
  dplyr::relocate(ret, dplyr::starts_with(".pred"))
}


augment_censored <- function(x, new_data, eval_time = NULL) {
  ret <- new_data
  if (parsnip:::spec_has_pred_type(x, "survival")) {
    parsnip:::.filter_eval_time(eval_time)
    ret <- dplyr::bind_cols(
      ret,
      predict(x, new_data = new_data, type = "survival", eval_time = eval_time)
    )
    # Add inverse probability weights when the outcome is present in new_data
    y_col <- parsnip:::.find_surv_col(new_data, fail = FALSE)
    if (length(y_col) != 0) {
      ret <- .censoring_weights_graf(x, ret)
    }
  }
  if (parsnip:::spec_has_pred_type(x, "time")) {
    ret <- dplyr::bind_cols(
      ret,
      predict(x, new_data = new_data, type = "time")
    )
  }
  dplyr::relocate(ret, dplyr::starts_with(".pred"))
}


