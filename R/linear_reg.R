#' General Interface for Linear Regression Models
#'
#' `linear_reg()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R, Stan, keras, or via Spark. The main
#'  arguments for the model are:
#' \itemize{
#'   \item \code{penalty}: The total amount of regularization
#'  in the model. Note that this must be zero for some engines.
#'   \item \code{mixture}: The mixture amounts of different types of
#'   regularization (see below). Note that this will be ignored for some engines.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and arguments can be
#'  set using `set_engine()`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param engine A character string for the method of fitting. Possible engines
#' are listed above. The default for this model is `"lm"`.
#' @param penalty A non-negative number representing the total
#'  amount of regularization (`glmnet`, `keras`, and `spark` only).
#'  For `keras` models, this corresponds to purely L2 regularization
#'  (aka weight decay) while the other models can be a combination
#'  of L1 and L2 (depending on the value of `mixture`; see below).
#' @param mixture A number between zero and one (inclusive) that is the
#'  proportion of L1 regularization (i.e. lasso) in the model. When
#'  `mixture = 1`, it is a pure lasso model while `mixture = 0` indicates that
#'  ridge regression is being used. (`glmnet` and `spark` only).
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `linear_reg()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"lm"`  (the default) or `"glmnet"`
#' \item \pkg{Stan}:  `"stan"`
#' \item \pkg{Spark}: `"spark"`
#' \item \pkg{keras}: `"keras"`
#' }
#'
#' For this model, other packages may add additional engines. Use
#' [show_engines()] to see the current set of engines.
#'
#' @includeRmd man/rmd/linear-reg.Rmd details
#'
#' @note For models created using the spark engine, there are
#'  several differences to consider. First, only the formula
#'  interface to via `fit()` is available; using `fit_xy()` will
#'  generate an error. Second, the predictions will always be in a
#'  spark table format. The names will be the same as documented but
#'  without the dots. Third, there is no equivalent to factor
#'  columns in spark tables so class predictions are returned as
#'  character columns. Fourth, to retain the model object for a new
#'  R session (via `save()`), the `model$fit` element of the `parsnip`
#'  object should be serialized via `ml_save(object$fit)` and
#'  separately saved to disk. In a new session, the object can be
#'  reloaded and reattached to the `parsnip` object.
#'
#' @seealso [fit()], [set_engine()], [update()]
#' @examples
#' show_engines("linear_reg")
#'
#' linear_reg()
#' # Parameters can be represented by a placeholder:
#' linear_reg(penalty = varying())
#' @export
#' @importFrom purrr map_lgl
linear_reg <-
  function(mode = "regression",
           engine = "lm",
           penalty = NULL,
           mixture = NULL) {

    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    new_model_spec(
      "linear_reg",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.linear_reg <- function(x, ...) {
  cat("Linear Regression Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}


#' @export
translate.linear_reg <- function(x, engine = x$engine, ...) {
  x <- translate.default(x, engine, ...)

  if (engine == "glmnet") {
    check_glmnet_penalty(x)
    if (any(names(x$eng_args) == "path_values")) {
      # Since we decouple the parsnip `penalty` argument from being the same
      # as the glmnet `lambda` value, `path_values` allows users to set the
      # path differently from the default that glmnet uses. See
      # https://github.com/tidymodels/parsnip/issues/431
      x$method$fit$args$lambda <- x$eng_args$path_values
      x$eng_args$path_values <- NULL
      x$method$fit$args$path_values <- NULL
    } else {
      # See discussion in https://github.com/tidymodels/parsnip/issues/195
      x$method$fit$args$lambda <- NULL
    }
    # Since the `fit` information is gone for the penalty, we need to have an
    # evaluated value for the parameter.
    x$args$penalty <- rlang::eval_tidy(x$args$penalty)
  }
  x
}


# ------------------------------------------------------------------------------

#' @method update linear_reg
#' @rdname parsnip_update
#' @export
update.linear_reg <-
  function(object,
           parameters = NULL,
           penalty = NULL, mixture = NULL,
           fresh = FALSE, ...) {

    eng_args <- update_engine_parameters(object$eng_args, ...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }
    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    args <- update_main_parameters(args, parameters)

    if (fresh) {
      object$args <- args
      object$eng_args <- eng_args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
      if (length(eng_args) > 0)
        object$eng_args[names(eng_args)] <- eng_args
    }

    new_model_spec(
      "linear_reg",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

check_args.linear_reg <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$penalty)) && any(args$penalty < 0))
    rlang::abort("The amount of regularization should be >= 0.")
  if (is.numeric(args$mixture) && (args$mixture < 0 | args$mixture > 1))
    rlang::abort("The mixture proportion should be within [0,1].")
  if (is.numeric(args$mixture) && length(args$mixture) > 1)
    rlang::abort("Only one value of `mixture` is allowed.")

  invisible(object)
}

# ------------------------------------------------------------------------------

organize_glmnet_pred <- function(x, object) {
  if (ncol(x) == 1) {
    res <- x[, 1]
    res <- unname(res)
  } else {
    n <- nrow(x)
    res <- utils::stack(as.data.frame(x))
    if (!is.null(object$spec$args$penalty))
      res$lambda <- rep(object$spec$args$penalty, each = n) else
        res$lambda <- rep(object$fit$lambda, each = n)
    res <- res[, colnames(res) %in% c("values", "lambda")]
  }
  res
}


# ------------------------------------------------------------------------------

# For `predict` methods that use `glmnet`, we have specific methods.
# Only one value of the penalty should be allowed when called by `predict()`:

check_penalty <- function(penalty = NULL, object, multi = FALSE) {

  if (is.null(penalty)) {
    penalty <- object$fit$lambda
  }

  # when using `predict()`, allow for a single lambda
  if (!multi) {
    if (length(penalty) != 1)
      rlang::abort(
        glue::glue(
          "`penalty` should be a single numeric value. `multi_predict()` ",
          "can be used to get multiple predictions per row of data.",
        )
      )
  }

  if (length(object$fit$lambda) == 1 && penalty != object$fit$lambda)
    rlang::abort(
      glue::glue(
        "The glmnet model was fit with a single penalty value of ",
        "{object$fit$lambda}. Predicting with a value of {penalty} ",
        "will give incorrect results from `glmnet()`."
      )
    )

  penalty
}

# ------------------------------------------------------------------------------
# glmnet call stack for linear regression using `predict` when object has
# classes "_elnet" and "model_fit":
#
#  predict()
# 	predict._elnet(penalty = NULL)   <-- checks and sets penalty
#    predict.model_fit()             <-- checks for extra vars in ...
#     predict_numeric()
#      predict_numeric._elnet()
#       predict_numeric.model_fit()
#        predict.elnet()


# glmnet call stack for linear regression using `multi_predict` when object has
# classes "_elnet" and "model_fit":
#
# 	multi_predict()
#    multi_predict._elnet(penalty = NULL)
#      predict._elnet(multi = TRUE)          <-- checks and sets penalty
#       predict.model_fit()                  <-- checks for extra vars in ...
#        predict_raw()
#         predict_raw._elnet()
#          predict_raw.model_fit(opts = list(s = penalty))
#           predict.elnet()


#' @export
predict._elnet <-
  function(object, new_data, type = NULL, opts = list(), penalty = NULL, multi = FALSE, ...) {
    if (any(names(enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    if (is.null(penalty) & !is.null(object$spec$args$penalty)) {
      penalty <- object$spec$args$penalty
    }

    object$spec$args$penalty <- check_penalty(penalty, object, multi)

    object$spec <- eval_args(object$spec)
    predict.model_fit(object, new_data = new_data, type = type, opts = opts, ...)
  }

#' @export
predict_numeric._elnet <- function(object, new_data, ...) {
  if (any(names(enquos(...)) == "newdata"))
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

  object$spec <- eval_args(object$spec)
  predict_numeric.model_fit(object, new_data = new_data, ...)
}

#' @export
predict_raw._elnet <- function(object, new_data, opts = list(), ...)  {
  if (any(names(enquos(...)) == "newdata"))
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

  object$spec <- eval_args(object$spec)
  opts$s <- object$spec$args$penalty
  predict_raw.model_fit(object, new_data = new_data, opts = opts, ...)
}

#' @importFrom dplyr full_join as_tibble arrange
#' @importFrom tidyr gather
#' @export
#'@rdname multi_predict
#' @param penalty A numeric vector of penalty values.
multi_predict._elnet <-
  function(object, new_data, type = NULL, penalty = NULL, ...) {
    if (any(names(enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    dots <- list(...)

    object$spec <- eval_args(object$spec)

    if (is.null(penalty)) {
      # See discussion in https://github.com/tidymodels/parsnip/issues/195
      if (!is.null(object$spec$args$penalty)) {
        penalty <- object$spec$args$penalty
      } else {
        penalty <- object$fit$lambda
      }
    }

    pred <- predict._elnet(object, new_data = new_data, type = "raw",
                           opts = dots, penalty = penalty, multi = TRUE)
    param_key <- tibble(group = colnames(pred), penalty = penalty)
    pred <- as_tibble(pred)
    pred$.row <- 1:nrow(pred)
    pred <- gather(pred, group, .pred, -.row)
    pred <- full_join(param_key, pred, by = "group")
    pred$group <- NULL
    pred <- arrange(pred, .row, penalty)
    .row <- pred$.row
    pred$.row <- NULL
    pred <- split(pred, .row)
    names(pred) <- NULL
    tibble(.pred = pred)
  }
