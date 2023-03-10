#' Ensembles of MARS models
#'
#' @description
#'
#' `bag_mars()` defines an ensemble of generalized linear models that use
#' artificial features for some predictors. These features resemble hinge
#' functions and the result is a model that is a segmented regression in small
#' dimensions. This function can fit classification and regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("bag_mars")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams mars
#'
#' @templateVar modeltype bag_mars
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("bag_mars")}
#' @export
bag_mars <-
  function(mode = "unknown",
           num_terms = NULL,
           prod_degree = NULL,
           prune_method = NULL,
           engine = "earth") {
    args <- list(
      num_terms   = enquo(num_terms),
      prod_degree  = enquo(prod_degree),
      prune_method  = enquo(prune_method)
    )

    new_model_spec(
      "bag_mars",
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

#' @method update bag_mars
#' @rdname parsnip_update
#' @inheritParams mars
#' @export
update.bag_mars <-
  function(object,
           parameters = NULL,
           num_terms = NULL, prod_degree = NULL, prune_method = NULL,
           fresh = FALSE, ...) {

    args <- list(
      num_terms   = enquo(num_terms),
      prod_degree  = enquo(prod_degree),
      prune_method  = enquo(prune_method)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "bag_mars",
      ...
    )
  }

# ------------------------------------------------------------------------------

set_new_model("bag_mars")
set_model_mode("bag_mars", "classification")
set_model_mode("bag_mars", "regression")
