#' Regularized discriminant analysis
#'
#' @description
#'
#' `discrim_regularized()` defines a model that estimates a multivariate
#'  distribution for the predictors separately for the data in each class. The
#'  structure of the model can be LDA, QDA, or some amalgam of the two. Bayes'
#'  theorem is used to compute the probability of each class, given the
#'  predictor values. This function can fit classification models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("discrim_regularized")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams nearest_neighbor
#' @inheritParams discrim_linear
#' @param frac_common_cov,frac_identity Numeric values between zero and one.
#'
#' @details
#' There are many ways of regularizing models. For example, one form of
#'  regularization is to penalize model parameters. Similarly, the classic
#'  James–Stein regularization approach shrinks the model structure to a less
#'  complex form.
#'
#' The model fits a very specific type of regularized model by Friedman (1989)
#'  that uses two types of regularization. One modulates how class-specific the
#'  covariance matrix should be. This allows the model to balance between LDA
#'  and QDA. The second regularization component shrinks the covariance matrix
#'  towards the identity matrix.
#'
#' For the penalization approach, [discrim_linear()] with a `mda` engine can be
#'  used. Other regularization methods can be used with [discrim_linear()] and
#'  [discrim_quad()] can used via the `sparsediscrim` engine for those functions.
#'
#' @templateVar modeltype discrim_regularized
#' @template spec-details
#'
#' @template spec-references
#'
#' @references
#' Friedman, J (1989). Regularized Discriminant Analysis. _Journal of the
#' American Statistical Association_, 84, 165-175.
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("discrim_regularized")}
#' @export
discrim_regularized <-
  function(mode = "classification", frac_common_cov = NULL, frac_identity = NULL,
           engine = "klaR") {

    args <- list(
      frac_common_cov = rlang::enquo(frac_common_cov),
      frac_identity = rlang::enquo(frac_identity)
    )

    new_model_spec(
      "discrim_regularized",
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

#' @method update discrim_regularized
#' @rdname parsnip_update
#' @inheritParams discrim_regularized
#' @export
update.discrim_regularized <-
  function(object,
           frac_common_cov = NULL,
           frac_identity = NULL,
           fresh = FALSE, ...) {

    args <- list(
      frac_common_cov = rlang::enquo(frac_common_cov),
      frac_identity = rlang::enquo(frac_identity)
    )

    update_spec(
      object = object,
      parameters = NULL,
      args_enquo_list = args,
      fresh = fresh,
      cls = "discrim_regularized",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
check_args.discrim_regularized <- function(object, call = rlang::caller_env()) {

  args <- lapply(object$args, rlang::eval_tidy)

  check_number_decimal(args$frac_common_cov, min = 0, max = 1, allow_null = TRUE, call = call, arg = "frac_common_cov")
  check_number_decimal(args$frac_identity, min = 0, max = 1, allow_null = TRUE, call = call, arg = "frac_identity")
  
  invisible(object)
}


# ------------------------------------------------------------------------------

set_new_model("discrim_regularized")
set_model_mode("discrim_regularized", "classification")

