# Define a generic to make multiple predictions for the same model object ------

#' Model predictions across many sub-models
#'
#' For some models, predictions can be made on sub-models in the model object.
#' @param object A [model fit][model_fit].
#' @param new_data A rectangular data object, such as a data frame.
#' @param type A single character value or `NULL`. Possible values are
#' `"numeric"`, `"class"`, `"prob"`, `"conf_int"`, `"pred_int"`, `"quantile"`,
#' or `"raw"`. When `NULL`, `predict()` will choose an appropriate value
#' based on the model's mode.
#' @param ... Optional arguments to pass to `predict.model_fit(type = "raw")`
#'  such as `type`.
#' @return A tibble with the same number of rows as the data being predicted.
#'  There is a list-column named `.pred` that contains tibbles with
#'  multiple rows per sub-model. Note that, within the tibbles, the column names
#'  follow the usual standard based on prediction `type` (i.e. `.pred_class` for
#'  `type = "class"` and so on).
#' @export
multi_predict <- function(object, ...) {
  if (inherits(object$fit, "try-error")) {
    cli::cli_warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }
  check_for_newdata(...)
  UseMethod("multi_predict")
}

#' @export
#' @rdname multi_predict
multi_predict.default <- function(object, ...) {
  cli::cli_abort(
    "No {.fun multi_predict} method exists for objects with classes
     {.cls {class(object)}}."
  )
}

#' @export
predict.model_spec <- function(object, ...) {
  cli::cli_abort(
    "You must {.fun fit} your {.help [model specification](parsnip::model_spec)}
     before you can use {.fun predict}."
  )
}

#' Tools for models that predict on sub-models
#'
#' `has_multi_predict()` tests to see if an object can make multiple
#'  predictions on submodels from the same object. `multi_predict_args()`
#'  returns the names of the arguments to `multi_predict()` for this model
#'  (if any).
#' @param object An object to test.
#' @param ... Not currently used.
#' @return `has_multi_predict()` returns single logical value while
#'  `multi_predict_args()` returns a character vector of argument names (or `NA`
#'  if none exist).
#' @keywords internal
#' @examplesIf !parsnip:::is_cran_check() & rlang::is_installed("kknn")
#' lm_model_idea <- linear_reg() |> set_engine("lm")
#' has_multi_predict(lm_model_idea)
#' lm_model_fit <- fit(lm_model_idea, mpg ~ ., data = mtcars)
#' has_multi_predict(lm_model_fit)
#'
#' multi_predict_args(lm_model_fit)
#'
#' library(kknn)
#'
#' knn_fit <-
#'   nearest_neighbor(mode = "regression", neighbors = 5) |>
#'   set_engine("kknn") |>
#'   fit(mpg ~ ., mtcars)
#'
#' multi_predict_args(knn_fit)
#'
#' multi_predict(knn_fit, mtcars[1, -1], neighbors = 1:4)$.pred
#' @export
has_multi_predict <- function(object, ...) {
  UseMethod("has_multi_predict")
}

#' @export
#' @rdname has_multi_predict
has_multi_predict.default <- function(object, ...) {
  FALSE
}

#' @export
#' @rdname has_multi_predict
has_multi_predict.model_fit <- function(object, ...) {
  existing_mthds <- utils::methods("multi_predict")
  tst <- paste0("multi_predict.", class(object))
  any(tst %in% existing_mthds)
}

#' @export
#' @rdname has_multi_predict
has_multi_predict.workflow <- function(object, ...) {
  has_multi_predict(object$fit$model$model)
}


#' @export
#' @rdname has_multi_predict
multi_predict_args <- function(object, ...) {
  UseMethod("multi_predict_args")
}

#' @export
#' @rdname has_multi_predict
multi_predict_args.default <- function(object, ...) {
  if (inherits(object, "model_fit")) {
    res <- multi_predict_args.model_fit(object, ...)
  } else {
    res <- NA_character_
  }
  res
}

#' @export
#' @rdname has_multi_predict
multi_predict_args.model_fit <- function(object, ...) {
  model_type <- class(object$spec)[1]
  arg_info <- get_from_env(paste0(model_type, "_args"))
  arg_info <- arg_info[arg_info$engine == object$spec$engine,]
  arg_info <- arg_info[arg_info$has_submodel,]

  if (nrow(arg_info) == 0) {
    res <- NA_character_
  } else {
    res <- arg_info[["parsnip"]]
  }

  res
}

#' @export
#' @rdname has_multi_predict
multi_predict_args.workflow <- function(object, ...) {
  object <- object$fit$model$model
}
