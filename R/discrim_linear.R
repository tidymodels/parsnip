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
#' @inheritParams boost_tree
#' @param mode A single character string for the type of model. The only
#'  possible value for this model is "classification".
#' @param penalty An non-negative number representing the amount of
#'  regularization used by some of the engines.
#' @param regularization_method A character string for the type of regularized
#'  estimation. Possible values are: "`diagonal`", "`min_distance`",
#'  "`shrink_cov`", and "`shrink_mean`" (`sparsediscrim` engine only).
#'
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
      method = NULL,
      engine = engine
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

check_args.discrim_linear <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$penalty)) && any(args$penalty < 0)) {
    stop("The amount of regularization should be >= 0", call. = FALSE)
  }

  invisible(object)
}

# ------------------------------------------------------------------------------

set_new_model("discrim_linear")
set_model_mode("discrim_linear", "classification")
