#' Model predictions
#'
#' Apply a model to create different types of predictions.
#'  `predict` can be used for all types of models and used the
#'  "type" argument for more specificity. Other functions can be
#'  used for each type of prediction (see Details).
#'
#' @param object An object of class `model_fit`
#' @param newdata A rectangular data object, such as a data frame.
#' @param type A single character value or `NULL`. Possible values
#'  are "numeric", "class", "probs", "link", or "raw" (the last two
#'  are not yet implemented). When `NULL`, `predict` will choose an
#'  appropriate value based on the model's mode.
#' @param ... Arguments to pass to other methods (not currently used).
#' @details If "type" is not supplied to `predict`, then a choice
#'  is made (`type = "numeric"` for regression models and 
#'  `type = "class"` for classification).
#'
#' `predict` is designed to provide a tidy results. There will be
#'  a tibble as many rows in the output as there are rows in
#'  `newdata` and the column names will be predictable. For numeric
#'  results with a single outcome, the tibble will have a `.pred`
#'  column and `.pred_Yname` for multivariate results. For
#'  hard class predictions, the column is named `.pred_class`
#'  and, when `type = "prob"`, the columns are 
#'  `.pred_classlevel`.
#' 
#' The more specific prediction functions (e.g. `predict_num`)
#'  return non-tibble results. `predict_num` generates a vector (for
#'  univariate models) or a data frame (multivariate).
#'  `predict_class` returns a factor and `predict_classprob` returns
#'  a data frame with columns for the factor levels.
#' @return See Details.
#' @examples 
#' library(dplyr)
#' 
#' lm_model <- 
#'   linear_reg() %>%
#'   fit(mpg ~ ., data = mtcars %>% slice(11:32), engine = "lm")
#' 
#' predict(lm_model, mtcars %>% slice(1:10) %>% select(-mpg))
#' predict_num(lm_model, mtcars %>% slice(1:10) %>% select(-mpg))
#' @importFrom stats predict
#' @method predict model_fit
#' @export predict.model_fit
#' @export
predict.model_fit <- function (object, newdata, type = NULL, ...) {
  type <- check_pred_type(object, type)
  res <- switch(
    type,
    numeric = predict_num(object = object, newdata = newdata),
    class   = predict_class(object = object, newdata = newdata),
    prob    = predict_classprob(object = object, newdata = newdata),
    stop("I don't know about type =", type, call. = FALSE)
  )
  res <- switch(
    type,
    numeric = format_num(res),
    class   = format_class(res),
    prob    = format_classprobs(res),
    stop("I don't know about type =", type, call. = FALSE)
  )
  res
}

# TODO: add a type for both class and probs

pred_types <- c("raw", "numeric", "class", "link", "prob")

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

format_num <- function(x) {
  if (isTRUE(ncol(x) > 1)) {
    x <- as_tibble(x)
    names(x) <- paste0(".pred_", names(x))
  } else {
    x <- tibble(.pred = x)
  }
  x
}

format_class <- function(x) {
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

prepare_data <- function(object, newdata) {
  fit_interface <- object$spec$method$fit$interface

  if (!all(is.na(object$preproc))) {
    # Translation code
    if (fit_interface == "formula") {
      newdata <- convert_xy_to_form_new(object$preproc, newdata)
    } else {
      newdata <- convert_form_to_xy_new(object$preproc, newdata)$x
    }
  }

  newdata
}

