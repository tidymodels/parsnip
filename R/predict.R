#' Model predictions
#'
#' Apply a model to create different types of predictions.
#'  `predict` can be used for all types of models and used the
#'  "type" argument for more specificity.
#'
#' @param object An object of class `model_fit`
#' @param new_data A rectangular data object, such as a data frame.
#' @param type A single character value or `NULL`. Possible values
#'  are "numeric", "class", "probs", "conf_int", "pred_int", or
#'  "raw". When `NULL`, `predict` will choose an appropriate value
#'  based on the model's mode.
#' @param opts A list of optional arguments to the underlying
#'  predict function that will be used when `type = "raw"`. The
#'  list should not include options for the model object or the
#'  new data being predicted.
#' @param ... Ignored. To pass arguments to pass to the underlying
#'  function when `predict.model_fit(type = "raw")`,
#' use the `opts` argument.
#' @details If "type" is not supplied to `predict`, then a choice
#'  is made (`type = "numeric"` for regression models and
#'  `type = "class"` for classification).
#'
#' `predict` is designed to provide a tidy result (see "Value"
#'  section below) in a tibble output format.
#'
#'  When using `type = "conf_int"` and `type = "pred_int"`, the options
#'   `level` and `std_error` can be used. The latter is a logical for an
#'   extra column of standard error values (if available).
#'
#' @return With the exception of `type = "raw"`, the results of
#'  `predict.model_fit` will be a tibble as many rows in the output
#'  as there are rows in `new_data` and the column names will be
#'  predictable.
#'
#' For numeric results with a single outcome, the tibble will have
#'  a `.pred` column and `.pred_Yname` for multivariate results.
#'
#' For hard class predictions, the column is named `.pred_class`
#'  and, when `type = "prob"`, the columns are `.pred_classlevel`.
#'
#' `type = "conf_int"` and `type = "pred_int"` return tibbles with
#'  columns `.pred_lower` and `.pred_upper` with an attribute for
#'  the confidence level. In the case where intervals can be
#'  produces for class probabilities (or other non-scalar outputs),
#'  the columns will be named `.pred_lower_classlevel` and so on.
#'
#' Using `type = "raw"` with `predict.model_fit` (or using
#'  `predict_raw`) will return the unadulterated results of the
#'  prediction function.
#'
#' In the case of Spark-based models, since table columns cannot
#'  contain dots, the same convention is used except 1) no dots
#'  appear in names and 2) vectors are never returned but
#'  type-specific prediction functions.
#'
#' @examples
#' library(dplyr)
#'
#' lm_model <-
#'   linear_reg() %>%
#'   fit(mpg ~ ., data = mtcars %>% slice(11:32), engine = "lm")
#'
#' pred_cars <-
#'   mtcars %>%
#'   slice(1:10) %>%
#'   select(-mpg)
#'
#' predict(lm_model, pred_cars)
#'
#' predict(
#'   lm_model,
#'   pred_cars,
#'   type = "conf_int",
#'   level = 0.90
#' )
#'
#' predict(
#'   lm_model,
#'   pred_cars,
#'   type = "raw",
#'   opts = list(type = "terms")
#' )
#' @importFrom safepredict safe_predict
#' @method predict model_fit
#' @export predict.model_fit
#' @export
predict.model_fit <- function (
  object, new_data, type = NULL, opts = list(), ...) {

  # TODO:
  # - make sure the mode matches up
  # - do form / x/y translate with prepare_newdata

  # is this enough preprocessing? or did I break something with the removals?
  new_data <- prepare_data(object, new_data)

  safepredict::safe_predict(
    object = object$fit,
    new_data = new_data,
    type = type,
    opts = opts,
    ...
  )
}

#' @export
multi_predict.model_fit <- function(object, ...) {
  multi_predict(object$fit, ...)
}

pred_types <- c("raw", "numeric", "class", "link", "prob", "conf_int", "pred_int")

#' @importFrom glue glue_collapse
check_pred_type <- function(object, type) {
  if (is.null(type)) {
    type <-
      switch(object$spec$mode,
             regression = "numeric",
             classification = "class",
             stop("Type should be 'regression' or 'classification'.", call. = FALSE))
  }
  if (!(type %in% pred_types))
    stop("'type' should be one of: ",
         glue_collapse(pred_types, sep = ", ", last = " and "),
         call. = FALSE)
  if (type == "numeric" & object$spec$mode != "regression")
    stop("For numeric predictions, the object should be a regression model.",
         call. = FALSE)
  if (type == "class" & object$spec$mode != "classification")
    stop("For class predictions, the object should be a classification model.",
         call. = FALSE)
  if (type == "prob" & object$spec$mode != "classification")
    stop("For probability predictions, the object should be a classification model.",
         call. = FALSE)
  type
}

make_pred_call <- function(x) {
  if ("pkg" %in% names(x$func))
    cl <-
      call2(x$func["fun"],!!!x$args, .ns = x$func["pkg"])
  else
    cl <-   call2(x$func["fun"],!!!x$args)

  cl
}

prepare_data <- function(object, new_data) {
  fit_interface <- object$spec$method$fit$interface

  if (!all(is.na(object$preproc))) {
    # Translation code
    if (fit_interface == "formula") {
      new_data <- convert_xy_to_form_new(object$preproc, new_data)
    } else {
      new_data <- convert_form_to_xy_new(object$preproc, new_data)$x
    }
  }

  new_data
}
