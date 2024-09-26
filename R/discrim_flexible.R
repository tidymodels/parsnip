#' Flexible discriminant analysis
#'
#' @description
#'
#' `discrim_flexible()` defines a model that fits a discriminant analysis model
#' that can use nonlinear features created using multivariate adaptive
#'  regression splines (MARS). This function can fit classification models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("discrim_flexible")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams nearest_neighbor
#' @inheritParams discrim_linear
#' @param num_terms The number of features that will be retained in the
#'    final model, including the intercept.
#' @param prod_degree The highest possible interaction degree.
#' @param prune_method The pruning method.
#'
#' @templateVar modeltype discrim_flexible
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("discrim_flexible")}
#'
#' @export
discrim_flexible <-
  function(mode = "classification", num_terms = NULL, prod_degree = NULL,
           prune_method = NULL, engine = "earth") {

    args <- list(
      num_terms    = enquo(num_terms),
      prod_degree  = enquo(prod_degree),
      prune_method = enquo(prune_method)
    )

    new_model_spec(
      "discrim_flexible",
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

#' Update a model specification
#' @param object A [model specification][model_spec].
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @method update discrim_flexible
#' @rdname parsnip_update
#' @inheritParams discrim_flexible
#' @export
update.discrim_flexible <-
  function(object,
           num_terms = NULL,
           prod_degree = NULL,
           prune_method = NULL,
           fresh = FALSE, ...) {

    args <- list(
      num_terms    = enquo(num_terms),
      prod_degree  = enquo(prod_degree),
      prune_method = enquo(prune_method)
    )

    update_spec(
      object = object,
      parameters = NULL,
      args_enquo_list = args,
      fresh = fresh,
      cls = "discrim_flexible",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
check_args.discrim_flexible <- function(object, call = rlang::caller_env()) {

  args <- lapply(object$args, rlang::eval_tidy)

  check_number_whole(args$prod_degree, min = 1, allow_null = TRUE, call = call, arg = "prod_degree")
  check_number_whole(args$num_terms, min = 1, allow_null = TRUE, call = call, arg = "num_terms")
  check_string(args$prune_method, allow_empty = FALSE, allow_null = TRUE, call = call, arg = "prune_method")

  invisible(object)
}

# ------------------------------------------------------------------------------

set_new_model("discrim_flexible")
set_model_mode("discrim_flexible", "classification")
