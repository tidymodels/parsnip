#' Poisson regression models
#'
#' @description
#'
#' `poisson_reg()` defines a generalized linear model for count data that follow
#' a Poisson distribution.

#' There are different ways to fit this model. See the engine-specific pages
#' for more details:
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("poisson_reg", "poissonreg")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param penalty A non-negative number representing the total
#'  amount of regularization (`glmnet` only).
#' @param mixture A number between zero and one (inclusive) that is the
#'  proportion of L1 regularization (i.e. lasso) in the model. When
#'  `mixture = 1`, it is a pure lasso model while `mixture = 0` indicates that
#'  ridge regression is being used. (`glmnet` and `spark` only).
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("poisson_reg", "poissonreg")}
#' @export
#' @importFrom purrr map_lgl
poisson_reg <-
  function(mode = "regression",
           penalty = NULL,
           mixture = NULL,
           engine = "glm") {

    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    new_model_spec(
      "poisson_reg",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.poisson_reg <- function(x, ...) {
  cat("Poisson Regression Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}


#' @export
translate.poisson_reg <- function(x, engine = x$engine, ...) {
  x <- translate.default(x, engine, ...)

  if (engine == "glmnet") {
    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    x$method$fit$args$lambda <- NULL
    # Since the `fit` infomration is gone for the penalty, we need to have an
    # evaludated value for the parameter.
    x$args$penalty <- rlang::eval_tidy(x$args$penalty)
  }

  x
}


# ------------------------------------------------------------------------------

#' @param object A boosted tree model specification.
#' @param parameters A 1-row tibble or named list with _main_
#'  parameters to update. If the individual arguments are used,
#'  these will supersede the values in `parameters`. Also, using
#'  engine arguments in this object will result in an error.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @return An updated model specification.
#' @examples
#' model <- poisson_reg(penalty = 10, mixture = 0.1)
#' model
#' update(model, penalty = 1)
#' update(model, penalty = 1, fresh = TRUE)
#' @method update poisson_reg
#' @rdname poisson_reg
#' @export
update.poisson_reg <-
  function(object,
           parameters = NULL,
           penalty = NULL, mixture = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)

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
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }

    new_model_spec(
      "poisson_reg",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

check_args.poisson_reg <- function(object) {

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

# ------------------------------------------------------------------------------
# glmnet call stack for poissom regression using `predict` when object has
# classes "_fishnet" and "model_fit":
#
#  predict()
# 	predict._fishnet(penalty = NULL)   <-- checks and sets penalty
#    predict.model_fit()             <-- checks for extra vars in ...
#     predict_numeric()
#      predict_numeric._fishnet()
#       predict_numeric.model_fit()
#        predict.fishnet()


# glmnet call stack for poisson regression using `multi_predict` when object has
# classes "_fishnet" and "model_fit":
#
# 	multi_predict()
#    multi_predict._fishnet(penalty = NULL)
#      predict._fishnet(multi = TRUE)          <-- checks and sets penalty
#       predict.model_fit()                  <-- checks for extra vars in ...
#        predict_raw()
#         predict_raw._fishnet()
#          predict_raw.model_fit(opts = list(s = penalty))
#           predict.fishnet()


#' @export
predict._fishnet <-
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
predict_numeric._fishnet <- function(object, new_data, ...) {
  if (any(names(enquos(...)) == "newdata"))
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

  object$spec <- eval_args(object$spec)
  predict_numeric.model_fit(object, new_data = new_data, ...)
}

#' Model predictions across many sub-models
#'
#' For some models, predictions can be made on sub-models in the model object.
#' @param object A `model_fit` object.
#' @param new_data A rectangular data object, such as a data frame.
#' @param opts A list of options..
#' @param ... Optional arguments to pass to `predict.model_fit(type = "raw")`
#'  such as `type`.
#' @return A tibble with the same number of rows as the data being predicted.
#'  There is a list-column named `.pred` that contains tibbles with
#'  multiple rows per sub-model.
#' @export
#' @keywords internal
predict_raw._fishnet <- function(object, new_data, opts = list(), ...)  {
  if (any(names(enquos(...)) == "newdata")) {
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
  }

  object$spec <- eval_args(object$spec)
  opts$s <- object$spec$args$penalty
  predict_raw.model_fit(object, new_data = new_data, opts = opts, ...)
}

#' @importFrom dplyr full_join as_tibble arrange
#' @importFrom tidyr gather
#' @export
#' @rdname predict_raw._fishnet
#' @param penalty A numeric vector of penalty values.
multi_predict._fishnet <-
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

    pred <- predict._fishnet(object, new_data = new_data, type = "raw",
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

# ------------------------------------------------------------------------------

set_new_model("poisson_reg")
set_model_mode("poisson_reg", "regression")
