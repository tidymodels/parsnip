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
#' reg_form <-
#'   linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ ., data = mtcars)
#' reg_xy <-
#'   linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit_xy(mtcars[, -1], mtcars$mpg)
#'
#' augment(reg_form, head(mtcars))
#' augment(reg_form, head(mtcars[, -1]))
#'
#' augment(reg_xy, head(mtcars))
#' augment(reg_xy, head(mtcars[, -1]))
#'
#' # ------------------------------------------------------------------------------
#'
#' data(two_class_dat, package = "modeldata")
#'
#' cls_form <-
#'   logistic_reg() %>%
#'   set_engine("glm") %>%
#'   fit(Class ~ ., data = two_class_dat)
#' cls_xy <-
#'   logistic_reg() %>%
#'   set_engine("glm") %>%
#'   fit_xy(two_class_dat[, -3],
#'   two_class_dat$Class)
#'
#' augment(cls_form, head(two_class_dat))
#' augment(cls_form, head(two_class_dat[, -3]))
#'
#' augment(cls_xy, head(two_class_dat))
#' augment(cls_xy, head(two_class_dat[, -3]))
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
  } else {
    new_data <-
      new_data %>%
      dplyr::bind_cols(
        predict(x, new_data = new_data, type = "class"),
        predict(x, new_data = new_data, type = "prob")
      )
  }
  new_data
}
