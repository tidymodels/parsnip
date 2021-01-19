#' Augment data with predictions
#'
#' `augment()` will add column(s) for predictions to the given data.
#'
#' For regression models, a `.pred` column is added. If `x` was created using
#' [fit()] and `new_data` contains the outcome column, a `.resid` column is
#' also added.
#'
#' For classification models, the results include a column called `.pred_class`
#' as well as class probability columns named `.pred_{level}`.
#' @param x A `model_fit` object produced by [fit()] or [fit_xy()].
#' @param new_data A data frame or matrix.
#' @param ... Not currently used.
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
augment.model_fit <- function(x, new_data, ...) {
  if (x$spec$mode == "regression") {
    new_data <-
      new_data %>%
      dplyr::bind_cols(
        predict(x, new_data = new_data)
      )
    if (length(x$preproc$y_var) > 0) {
      y_nm <- x$preproc$y_var
      if (any(names(new_data) == y_nm)) {
        new_data <- dplyr::mutate(new_data, .resid = !!rlang::sym(y_nm) - .pred)
      }
    }
  } else if (x$spec$mode == "classification") {
    new_data <-
      new_data %>%
      dplyr::bind_cols(
        predict(x, new_data = new_data, type = "class"),
        predict(x, new_data = new_data, type = "prob")
      )
  } else {
    rlang::abort(paste("Unknown mode:", x$spec$mode))
  }
  new_data
}
