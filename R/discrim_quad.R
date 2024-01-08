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
#' @inheritParams nearest_neighbor
#' @param mode A single character string for the type of model. The only
#'  possible value for this model is "classification".
#' @param regularization_method A character string for the type of regularized
#'  estimation. Possible values are: "`diagonal`", "`shrink_cov`", and
#'  "`shrink_mean`" (`sparsediscrim` engine only).
#'
#' @templateVar modeltype discrim_quad
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
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
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

    args <- list(regularization_method = rlang::enquo(regularization_method))

    update_spec(
      object = object,
      parameters = NULL,
      args_enquo_list = args,
      fresh = fresh,
      cls = "discrim_quad",
      ...
    )
  }

# ------------------------------------------------------------------------------

set_new_model("discrim_quad")
set_model_mode("discrim_quad", "classification")

