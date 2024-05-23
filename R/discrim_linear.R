#' Linear discriminant analysis
#'
#' @description
#'
#' `discrim_linear()` defines a model that estimates a multivariate
#'  distribution for the predictors separately for the data in each class
#'  (usually Gaussian with a common covariance matrix). Bayes' theorem is used
#'  to compute the probability of each class, given the predictor values. This
#'  function can fit classification models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("discrim_linear")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams nearest_neighbor
#' @param mode A single character string for the type of model. The only
#'  possible value for this model is "classification".
#' @param penalty An non-negative number representing the amount of
#'  regularization used by some of the engines.
#' @param regularization_method A character string for the type of regularized
#'  estimation. Possible values are: "`diagonal`", "`min_distance`",
#'  "`shrink_cov`", and "`shrink_mean`" (`sparsediscrim` engine only).
#'
#' @templateVar modeltype discrim_linear
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("discrim_linear")}
#' @export
discrim_linear <-
  function(mode = "classification", penalty = NULL, regularization_method = NULL,
           engine = "MASS") {

    args <- list(
      penalty = rlang::enquo(penalty),
      regularization_method = rlang::enquo(regularization_method)
    )

    new_model_spec(
      "discrim_linear",
      args = args,
      eng_args = NULL,
      mode = mode,
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
  }

# ------------------------------------------------------------------------------

#' @method update discrim_linear
#' @rdname parsnip_update
#' @inheritParams discrim_linear
#' @export
update.discrim_linear <-
  function(object,
           penalty = NULL,
           regularization_method = NULL,
           fresh = FALSE, ...) {

    args <- list(
      penalty = rlang::enquo(penalty),
      regularization_method = rlang::enquo(regularization_method)
    )

    update_spec(
      object = object,
      parameters = NULL,
      args_enquo_list = args,
      fresh = fresh,
      cls = "discrim_linear",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
check_args.discrim_linear <- function(object, call = rlang::caller_env()) {

  args <- lapply(object$args, rlang::eval_tidy)

  check_number_decimal(args$penalty, min = 0, allow_null = TRUE, call = call, arg = "penalty")

  invisible(object)
}

# ------------------------------------------------------------------------------

set_new_model("discrim_linear")
set_model_mode("discrim_linear", "classification")
