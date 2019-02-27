#' Model predictions
#'
#' Apply a model to create different types of predictions.
#'  `predict()` can be used for all types of models and used the
#'  "type" argument for more specificity.
#'
#' @param object An object of class `model_fit`
#' @param new_data A rectangular data object, such as a data frame.
#' @param type A single character value or `NULL`. Possible values
#'  are "numeric", "class", "prob", "conf_int", "pred_int", "quantile",
#'  or "raw". When `NULL`, `predict()` will choose an appropriate value
#'  based on the model's mode.
#' @param opts A list of optional arguments to the underlying
#'  predict function that will be used when `type = "raw"`. The
#'  list should not include options for the model object or the
#'  new data being predicted.
#' @param ... Arguments to the underlying model's prediction
#'  function cannot be passed here (see `opts`). There are some
#'  `parsnip` related options that can be passed, depending on the
#'  value of `type`. Possible arguments are:
#'  \itemize{
#'     \item `level`: for `type`s of "conf_int" and "pred_int" this
#'            is the parameter for the tail area of the intervals
#'            (e.g. confidence level for confidence intervals).
#'            Default value is 0.95.
#'     \item `std_error`: add the standard error of fit or
#'            prediction for `type`s of "conf_int" and "pred_int".
#'            Default value is `FALSE`.
#'     \item `quantile`: the quantile(s) for quantile regression
#'            (not implemented yet)
#'     \item `time`: the time(s) for hazard probability estimates
#'            (not implemented yet)
#'  }
#' @details If "type" is not supplied to `predict()`, then a choice
#'  is made (`type = "numeric"` for regression models and
#'  `type = "class"` for classification).
#'
#' `predict()` is designed to provide a tidy result (see "Value"
#'  section below) in a tibble output format.
#'
#'  When using `type = "conf_int"` and `type = "pred_int"`, the options
#'   `level` and `std_error` can be used. The latter is a logical for an
#'   extra column of standard error values (if available).
#'
#' @return With the exception of `type = "raw"`, the results of
#'  `predict.model_fit()` will be a tibble as many rows in the output
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
#' Quantile predictions return a tibble with a column `.pred`, which is
#'  a list-column. Each list element contains a tibble with columns
#'  `.pred` and `.quantile` (and perhaps other columns).
#'
#' Using `type = "raw"` with `predict.model_fit()` will return
#'  the unadulterated results of the prediction function.
#'
#' In the case of Spark-based models, since table columns cannot
#'  contain dots, the same convention is used except 1) no dots
#'  appear in names and 2) vectors are never returned but
#'  type-specific prediction functions.
#'
#' When the model fit failed and the error was captured, the
#'  `predict()` function will return the same structure as above but
#'  filled with missing values. This does not currently work for
#'  multivariate models.
#' @examples
#' library(dplyr)
#'
#' lm_model <-
#'   linear_reg() %>%
#'   set_engine("lm") %>%
#'   fit(mpg ~ ., data = mtcars %>% slice(11:32))
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
#' @importFrom stats predict
#' @method predict model_fit
#' @export predict.model_fit
#' @export
predict.model_fit <- function(object, new_data, type = NULL, opts = list(), ...) {
  the_dots <- enquos(...)
  if (any(names(the_dots) == "newdata"))
    stop("Did you mean to use `new_data` instead of `newdata`?", call. = FALSE)

  if (inherits(object$fit, "try-error")) {
    warning("Model fit failed; cannot make predictions.", call. = FALSE)
    return(NULL)
  }

  other_args <- c("level", "std_error", "quantile") # "time" for survival probs later
  is_pred_arg <- names(the_dots) %in% other_args
  if (any(!is_pred_arg)) {
    bad_args <- names(the_dots)[!is_pred_arg]
    bad_args <- paste0("`", bad_args, "`", collapse = ", ")
    stop("The ellipses are not used to pass args to the model function's ",
         "predict function. These arguments cannot be used: ",
         bad_args, call. = FALSE)
  }

  type <- check_pred_type(object, type)
  if (type != "raw" && length(opts) > 0)
    warning("`opts` is only used with `type = 'raw'` and was ignored.")
  res <- switch(
    type,
    numeric  = predict_numeric(object = object, new_data = new_data, ...),
    class    = predict_class(object = object, new_data = new_data, ...),
    prob     = predict_classprob(object = object, new_data = new_data, ...),
    conf_int = predict_confint(object = object, new_data = new_data, ...),
    pred_int = predict_predint(object = object, new_data = new_data, ...),
    quantile = predict_quantile(object = object, new_data = new_data, ...),
    raw      = predict_raw(object = object, new_data = new_data, opts = opts, ...),
    stop("I don't know about type = '", "'", type, call. = FALSE)
  )

  if (!inherits(res, "tbl_spark")) {
    res <- switch(
      type,
      numeric = format_num(res),
      class   = format_class(res),
      prob    = format_classprobs(res),
      res
    )
  }
  res
}

pred_types <-
  c("raw", "numeric", "class", "link", "prob", "conf_int", "pred_int", "quantile")

#' @importFrom glue glue_collapse
check_pred_type <- function(object, type) {
  if (is.null(type)) {
    type <-
      switch(object$spec$mode,
             regression = "numeric",
             classification = "class",
             stop("`type` should be 'regression' or 'classification'.", call. = FALSE))
  }
  if (!(type %in% pred_types))
    stop("`type` should be one of: ",
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

format_num <- function(x) {
  if (inherits(x, "tbl_spark"))
    return(x)

  if (isTRUE(ncol(x) > 1)) {
    x <- as_tibble(x)
    names(x) <- paste0(".pred_", names(x))
  } else {
    x <- tibble(.pred = x)
  }

  x
}

format_class <- function(x) {
  if (inherits(x, "tbl_spark"))
    return(x)

  tibble(.pred_class = x)
}

format_classprobs <- function(x) {
  x <- as_tibble(x)
  names(x) <- paste0(".pred_", names(x))
  x
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

# Define a generic to make multiple predictions for the same model object ------

#' Model predictions across many sub-models
#'
#' For some models, predictions can be made on sub-models in the model object.
#' @param object A `model_fit` object.
#' @param ... Optional arguments to pass to `predict.model_fit(type = "raw")`
#'  such as `type`.
#' @return A tibble with the same number of rows as the data being predicted.
#'  Mostly likely, there is a list-column named `.pred` that is a tibble with
#'  multiple rows per sub-model.
#' @keywords internal
#' @export
multi_predict <- function(object, ...) {
  if (inherits(object$fit, "try-error")) {
    warning("Model fit failed; cannot make predictions.", call. = FALSE)
    return(NULL)
  }
  UseMethod("multi_predict")
}

#' @keywords internal
#' @export
#' @rdname multi_predict
multi_predict.default <- function(object, ...)
  stop("No `multi_predict` method exists for objects with classes ",
        paste0("'", class(), "'", collapse = ", "), call. = FALSE)

#' @export
predict.model_spec <- function(object, ...) {
  stop("You must use `fit()` on your model specification before you can use `predict()`.", call. = FALSE)
}
