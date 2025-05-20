#' Model predictions
#'
#' Apply a model to create different types of predictions.
#'  `predict()` can be used for all types of models and uses the
#'  "type" argument for more specificity.
#'
#' @param object A [model fit][model_fit].
#' @param new_data A rectangular data object, such as a data frame.
#' @param type A single character value or `NULL`. Possible values
#'   are `"numeric"`, `"class"`, `"prob"`, `"conf_int"`, `"pred_int"`,
#'   `"quantile"`, `"time"`, `"hazard"`, `"survival"`, or `"raw"`. When `NULL`,
#'  `predict()` will choose an appropriate value based on the model's mode.
#' @param opts A list of optional arguments to the underlying
#'  predict function that will be used when `type = "raw"`. The
#'  list should not include options for the model object or the
#'  new data being predicted.
#' @param ... Additional `parsnip`-related options, depending on the
#'  value of `type`. Arguments to the underlying model's prediction
#'  function cannot be passed here (use the `opts` argument instead).
#'  Possible arguments are:
#'  \itemize{
#'     \item `interval`: for `type` equal to `"survival"` or `"quantile"`, should
#'            interval estimates be added, if available? Options are `"none"`
#'            and `"confidence"`.
#'     \item `level`: for `type` equal to `"conf_int"`, `"pred_int"`, or `"survival"`,
#'            this is the parameter for the tail area of the intervals
#'            (e.g. confidence level for confidence intervals).
#'            Default value is `0.95`.
#'     \item `std_error`: for `type` equal to `"conf_int"` or `"pred_int"`, add
#'            the standard error of fit or prediction (on the scale of the
#'            linear predictors). Default value is `FALSE`.
#'     \item `quantile`: for `type` equal to `quantile`, the quantiles of the
#'            distribution. Default is `(1:9)/10`.
#'     \item `eval_time`: for `type` equal to `"survival"` or `"hazard"`, the
#'            time points at which the survival probability or hazard is estimated.
#'  }
#' @details For `type = NULL`, `predict()` uses
#'
#'   * `type = "numeric"` for regression models,
#'   * `type = "class"` for classification, and
#'   * `type = "time"` for censored regression.
#'
#'  ## Interval predictions
#'
#'  When using `type = "conf_int"` and `type = "pred_int"`, the options
#'   `level` and `std_error` can be used. The latter is a logical for an
#'   extra column of standard error values (if available).
#'
#'  ## Censored regression predictions
#'
#' For censored regression, a numeric vector for `eval_time` is required when
#' survival or hazard probabilities are requested. The time values are required
#' to be unique, finite, non-missing, and non-negative. The `predict()`
#' functions will adjust the values to fit this specification by removing
#' offending points (with a warning).
#'
#' `predict.model_fit()` does not require the outcome to be present. For
#' performance metrics on the predicted survival probability, inverse probability
#' of censoring weights (IPCW) are required (see the `tidymodels.org` reference
#' below). Those require the outcome and are thus not returned by `predict()`.
#' They can be added via [augment.model_fit()] if `new_data` contains a column
#' with the outcome as a `Surv` object.
#'
#' Also, when `type = "linear_pred"`, censored regression models will by default
#' be formatted such that the linear predictor _increases_ with time. This may
#' have the opposite sign as what the underlying model's `predict()` method
#' produces. Set `increasing = FALSE` to suppress this behavior.
#'
#' @return With the exception of `type = "raw"`, the result of
#'  `predict.model_fit()`
#'
#'  * is a tibble
#'  * has as many rows as there are rows in `new_data`
#'  * has standardized column names, see below:
#'
#' For `type = "numeric"`, the tibble has a `.pred` column for a single
#' outcome and `.pred_Yname` columns for a multivariate outcome.
#'
#' For `type = "class"`, the tibble has a `.pred_class` column.
#'
#' For `type = "prob"`, the tibble has `.pred_classlevel` columns.
#'
#' For `type = "conf_int"` and `type = "pred_int"`, the tibble has
#' `.pred_lower` and `.pred_upper` columns with an attribute for
#' the confidence level. In the case where intervals can be
#' produces for class probabilities (or other non-scalar outputs),
#' the columns are named `.pred_lower_classlevel` and so on.
#'
#' For `type = "quantile"`, the tibble has a `.pred` column, which is
#'  a list-column. Each list element contains a tibble with columns
#'  `.pred` and `.quantile` (and perhaps other columns).
#'
#' For `type = "time"`, the tibble has a `.pred_time` column.
#'
#' For `type = "survival"`, the tibble has a `.pred` column, which is
#'  a list-column. Each list element contains a tibble with columns
#'  `.eval_time` and `.pred_survival` (and perhaps other columns).
#'
#' For `type = "hazard"`, the tibble has a `.pred` column, which is
#'  a list-column. Each list element contains a tibble with columns
#'  `.eval_time` and `.pred_hazard` (and perhaps other columns).
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
#' @references
#' \url{https://www.tidymodels.org/learn/statistics/survival-metrics/}
#' @examplesIf !parsnip:::is_cran_check()
#' library(dplyr)
#'
#' lm_model <-
#'   linear_reg() |>
#'   set_engine("lm") |>
#'   fit(mpg ~ ., data = mtcars |> dplyr::slice(11:32))
#'
#' pred_cars <-
#'   mtcars |>
#'   dplyr::slice(1:10) |>
#'   dplyr::select(-mpg)
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
#' @method predict model_fit
#' @export predict.model_fit
#' @export
predict.model_fit <- function(object, new_data, type = NULL, opts = list(), ...) {
  if (inherits(object$fit, "try-error")) {
    cli::cli_warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  check_installs(object$spec)
  load_libs(object$spec, quiet = TRUE)

  type <- check_pred_type(object, type)
  if (type != "raw" && length(opts) > 0) {
    cli::cli_warn("{.arg opts} is only used with `type = 'raw'` and was ignored.")
  }
  check_pred_type_dots(object, type, ...)

  new_data <- to_sparse_data_frame(new_data, object)

  res <- switch(
    type,
    numeric     = predict_numeric(object = object, new_data = new_data, ...),
    class       = predict_class(object = object, new_data = new_data, ...),
    prob        = predict_classprob(object = object, new_data = new_data, ...),
    conf_int    = predict_confint(object = object, new_data = new_data, ...),
    pred_int    = predict_predint(object = object, new_data = new_data, ...),
    quantile    = predict_quantile(object = object, new_data = new_data, ...),
    time        = predict_time(object = object, new_data = new_data, ...),
    survival    = predict_survival(object = object, new_data = new_data, ...),
    linear_pred = predict_linear_pred(object = object, new_data = new_data, ...),
    hazard      = predict_hazard(object = object, new_data = new_data, ...),
    raw         = predict_raw(object = object, new_data = new_data, opts = opts, ...),
    cli::cli_abort("Unknown prediction {.arg type} '{type}'.")
  )
  if (!inherits(res, "tbl_spark")) {
    res <- switch(
      type,
      numeric     = format_num(res),
      class       = format_class(res),
      prob        = format_classprobs(res),
      time        = format_time(res),
      survival    = format_survival(res),
      hazard      = format_hazard(res),
      linear_pred = format_linear_pred(res),
      res
    )
  }
  res
}

check_pred_type <- function(object, type, ..., call = rlang::caller_env()) {
  if (is.null(type)) {
    type <-
      switch(
        object$spec$mode,
        regression = "numeric",
        classification = "class",
        "censored regression" = "time",
        "quantile regression" = "quantile",
        cli::cli_abort(
          "{.arg type} should be one of {.or {.val {all_modes}}}.",
          call = call
        )
      )
  }

  if (!(type %in% pred_types))
    cli::cli_abort(
      "{.arg type} should be one of {.or {.arg {pred_types}}}.",
      call = call
    )

  switch(
    type,
    "numeric" = if (object$spec$mode != "regression") {
      cli::cli_abort(
        "For numeric predictions, the object should be a regression model.",
        call = call
      )
    },
    "class" = if (object$spec$mode != "classification") {
      cli::cli_abort(
        "For class predictions, the object should be a classification model.",
        call = call
      )
    },
    "prob" = if (object$spec$mode != "classification") {
      cli::cli_abort(
        "For probability predictions, the object should be a classification model.",
        call = call
      )
    },
    "time" = if (object$spec$mode != "censored regression") {
      cli::cli_abort(
        "For event time predictions, the object should be a censored regression.",
        call = call
      )
    },
    "survival" = if (object$spec$mode != "censored regression") {
      cli::cli_abort(
        "For survival probability predictions, the object should be a censored regression.",
        call = call
      )
    },
    "hazard" = if (object$spec$mode != "censored regression") {
      cli::cli_abort(
        "For hazard predictions, the object should be a censored regression.",
        call = call
      )
    },
    "linear_pred" = if (object$spec$mode != "censored regression") {
      cli::cli_abort(
        "For the linear predictor, the object should be a censored regression.",
        call = call
      )
    }
  )

  # TODO check for ... options when not the correct type
  type
}

#' Internal functions that format predictions
#'
#' These are used to ensure that we have appropriate column names inside of
#' tibbles.
#'
#' @param x A data frame or vector (depending on the context and function).
#' @param col_name A string for a prediction column name.
#' @param overwrite A logical for whether to overwrite the column name.
#' @return A tibble
#' @keywords internal
#' @name format-internals
#' @export

format_num <- function(x) {
  if (inherits(x, "tbl_spark")) {
    return(x)
  }
  ensure_parsnip_format(x, ".pred", overwrite = FALSE)
}

#' @rdname format-internals
#' @export
format_class <- function(x) {
  if (inherits(x, "tbl_spark")) {
    return(x)
  }
  ensure_parsnip_format(x, ".pred_class")
}

#' @rdname format-internals
#' @export
format_classprobs <- function(x) {
  if (!any(grepl("^\\.pred_", names(x)))) {
    names(x) <- paste0(".pred_", names(x))
  }
  if (!tibble::is_tibble(x)) {
    x <- as_tibble(x)
  }

  for (i in seq_along(x)) {
    names(x[[i]]) <- NULL
  }

  x
}

#' @rdname format-internals
#' @export
format_time <- function(x) {
  ensure_parsnip_format(x, ".pred_time", overwrite = FALSE)
}

#' @rdname format-internals
#' @export
format_survival <- function(x) {
  ensure_parsnip_format(x, ".pred")
}

#' @rdname format-internals
#' @export
format_linear_pred <- function(x) {
  if (inherits(x, "tbl_spark")){
    return(x)
  }
  ensure_parsnip_format(x, ".pred_linear_pred")
}

#' @rdname format-internals
#' @export
format_hazard <- function(x) {
  ensure_parsnip_format(x, ".pred")
}

#' @export
#' @rdname format-internals
#' @keywords internal
ensure_parsnip_format <- function(x, col_name, overwrite = TRUE) {
  if (isTRUE(ncol(x) > 1) | is.data.frame(x)) {
    x <- tibble::new_tibble(x)
    if (!any(grepl(paste0("^\\", col_name), names(x)))) {
      if (overwrite) {
        names(x) <- col_name
      } else {
        names(x) <- paste(col_name, names(x), sep = "_")
      }
    }
  } else {
    x <- tibble::new_tibble(vctrs::df_list(unname(x), .name_repair = "minimal"),
                            nrow = length(x))
    names(x) <- col_name
    x
  }
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

check_pred_type_dots <- function(object, type, ..., call = rlang::caller_env()) {
  the_dots <- list(...)
  nms <- names(the_dots)

  # ----------------------------------------------------------------------------

  check_for_newdata(..., call = call)

  # ----------------------------------------------------------------------------

  other_args <- c("interval", "level", "std_error", "quantile_levels",
                  "time", "eval_time", "increasing")

  eval_time_types <- c("survival", "hazard")

  is_pred_arg <- names(the_dots) %in% other_args
  if (any(!is_pred_arg)) {
    bad_args <- names(the_dots)[!is_pred_arg]
    bad_args <- paste0("`", bad_args, "`", collapse = ", ")
    cli::cli_abort(
        "The ellipses are not used to pass args to the model function's
         predict function. These arguments cannot be used: {.val bad_args}",
         call = call
    )
  }

  # ----------------------------------------------------------------------------
  # places where eval_time should not be given
  if (any(nms == "eval_time") & !type %in% c("survival", "hazard")) {
    cli::cli_abort(
      "{.arg eval_time} should only be passed to {.fn predict} when \\
       {.arg type} is one of {.or {.val {eval_time_types}}}.",
       call = call
     )


  }
  if (any(nms == "time") & !type %in% c("survival", "hazard")) {
    cli::cli_abort(
      "{.arg time} should only be passed to {.fn predict} when {.arg type} is
       one of {.or {.val {eval_time_types}}}.",
      call = call
    )
  }
  # when eval_time should be passed
  if (!any(nms %in% c("eval_time", "time")) & type %in% c("survival", "hazard")) {
  cli::cli_abort(
    "When using {.arg type} values of {.or {.val {eval_time_types}}} a numeric
     vector {.arg eval_time} should also be given.",
    call = call
  )
  }

  # `increasing` only applies to linear_pred for censored regression
  if (any(nms == "increasing") &
      !(type == "linear_pred" &
        object$spec$mode == "censored regression")) {
    cli::cli_abort(
      "{.arg increasing} only applies to predictions of
       type 'linear_pred' for the mode censored regression.",
      call = call
    )

  }

  invisible(TRUE)
}


#' Prepare data based on parsnip encoding information
#' @param object A parsnip model object
#' @param new_data A data frame
#' @return A data frame or matrix
#' @keywords internal
#' @export
prepare_data <- function(object, new_data) {
  preproc_names <- names(object$preproc)
  translate_from_formula_to_xy <- any(preproc_names == "terms", na.rm = TRUE)
  translate_from_xy_to_formula <- any(preproc_names == "x_var", na.rm = TRUE)
  # For backwards compatibility, only do this if `y_var` is missing and
  # `x_names` is present
  translate_from_xy_to_xy <- any(preproc_names == "x_names", na.rm = TRUE) &&
    identical(object$preproc$y_var, character(0))

  if (translate_from_formula_to_xy) {
    new_data <- .convert_form_to_xy_new(object$preproc, new_data)$x
  } else if (translate_from_xy_to_formula) {
    new_data <- .convert_xy_to_form_new(object$preproc, new_data)
  } else if (translate_from_xy_to_xy) {
    new_data <- new_data[, object$preproc$x_names, drop = FALSE]
  }

  encodings <- get_encoding(class(object$spec)[1])
  remove_intercept <-
    vctrs::vec_slice(
      encodings$remove_intercept,
      encodings$mode == object$spec$mode &
        encodings$engine == object$spec$engine
    )

  if (remove_intercept & any(grepl("Intercept", names(new_data)))) {
    new_data <- new_data[, colnames(new_data) != "(Intercept)", drop = FALSE]
  }

  if (allow_sparse(object) && inherits(new_data, "dgCMatrix")) {
    return(new_data)
  }
  if (allow_sparse(object) && sparsevctrs::has_sparse_elements(new_data)) {
    new_data <- sparsevctrs::coerce_to_sparse_matrix(new_data)
    return(new_data)
  }

  fit_interface <- object$spec$method$fit$interface
  switch(
    fit_interface,
    none = new_data,
    data.frame = as.data.frame(new_data),
    matrix = as.matrix(new_data),
    new_data
  )
}

