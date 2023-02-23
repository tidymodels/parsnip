#' Linear regression
#'
#' @description
#'
#' `linear_reg()` defines a model that can predict numeric values from
#' predictors using a linear function. This function can fit regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("linear_reg")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting. Possible engines are listed below. The default for this
#'  model is `"lm"`.
#' @param penalty A non-negative number representing the total
#'  amount of regularization (specific engines only).
#' @param mixture A number between zero and one (inclusive) denoting the
#'  proportion of L1 regularization (i.e. lasso) in the model.
#'
#'  * `mixture = 1` specifies a pure lasso model,
#'  * `mixture = 0`  specifies a ridge regression model, and
#'  * `0 < mixture < 1` specifies an elastic net model, interpolating lasso and ridge.
#'
#'  Available for specific engines only.
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("linear_reg")}
#'
#' @examples
#' show_engines("linear_reg")
#'
#' linear_reg()
#' @export
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
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
  }

#' @export
translate.linear_reg <- function(x, engine = x$engine, ...) {
  x <- translate.default(x, engine, ...)

  if (engine == "glmnet") {
    # See https://parsnip.tidymodels.org/reference/glmnet-details.html
    .check_glmnet_penalty_fit(x)
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

    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "linear_reg",
      ...
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

#' Organize glmnet predictions
#'
#' This function is for developer use and organizes predictions from glmnet
#' models.
#'
#' @param x Predictions as returned by the `predict()` method for glmnet models.
#' @param object An object of class `model_fit`.
#'
#' @rdname glmnet_helpers_prediction
#' @keywords internal
#' @export
.organize_glmnet_pred <- function(x, object) {
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

#' @export
predict._elnet <- predict_glmnet

#' @export
predict_numeric._elnet <- predict_numeric_glmnet

#' @export
predict_raw._elnet <- predict_raw_glmnet

#' @export
#'@rdname multi_predict
#' @param penalty A numeric vector of penalty values.
multi_predict._elnet <- multi_predict_glmnet

format_glmnet_multi_linear_reg <- function(pred, penalty) {
  param_key <- tibble(group = colnames(pred), penalty = penalty)
  pred <- as_tibble(pred)
  pred$.row <- 1:nrow(pred)
  pred <- gather(pred, group, .pred, -.row)
  if (utils::packageVersion("dplyr") >= "1.0.99.9000") {
    pred <- full_join(param_key, pred, by = "group", multiple = "all")
  } else {
    pred <- full_join(param_key, pred, by = "group")
  }
  pred$group <- NULL
  pred <- arrange(pred, .row, penalty)
  .row <- pred$.row
  pred$.row <- NULL
  pred <- split(pred, .row)
  names(pred) <- NULL
  tibble(.pred = pred)
}
