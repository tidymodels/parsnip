#' Quadratic discriminant analysis
#'
#' @description
#'
#' `discrim_quad()` defines a model that estimates a multivariate
#'  distribution for the predictors separately for the data in each class
#'  (usually Gaussian with separate covariance matrices). Bayes' theorem is used
#'  to compute the probability of each class, given the predictor values. This
#'  function can fit classification models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("discrim_quad")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @param mode A single character string for the type of model. The only
#'  possible value for this model is "classification".
#' @param regularization_method A character string for the type of regularized
#'  estimation. Possible values are: "`diagonal`", "`shrink_cov`", and
#'  "`shrink_mean`" (`sparsediscrim` engine only).
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("discrim_quad")}
#' @export
discrim_quad <-
  function(mode = "classification", regularization_method = NULL, engine = "MASS") {

    args <- list(regularization_method = rlang::enquo(regularization_method))

    new_model_spec(
      "discrim_quad",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.discrim_quad <- function(x, ...) {
  cat("Quadratic Discriminant Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update discrim_quad
#' @rdname parsnip_update
#' @inheritParams discrim_quad
#' @export
update.discrim_quad <-
  function(object,
           regularization_method = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)
    args <- list(regularization_method = rlang::enquo(regularization_method))

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
      "discrim_quad",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

set_new_model("discrim_quad")
set_model_mode("discrim_quad", "classification")

