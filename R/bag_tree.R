#' Ensembles of decision trees
#'
#' @description
#'
#' `bag_tree()` defines an ensemble of decision trees. This function can fit
#'  classification, regression, and censored regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("bag_tree")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @inheritParams decision_tree
#' @param class_cost A non-negative scalar for a class cost (where a cost of 1
#' means no extra cost). This is useful for when the first level of the outcome
#' factor is the minority class. If this is not the case, values between zero
#' and one can be used to bias to the second level of the factor.
#'
#' @templateVar modeltype bag_tree
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("bag_tree")}
#' @export
bag_tree <-
  function(mode = "unknown",
           cost_complexity = 0,
           tree_depth = NULL,
           min_n = 2,
           class_cost = NULL,
           engine = "rpart") {
    args <- list(
      cost_complexity   = enquo(cost_complexity),
      tree_depth  = enquo(tree_depth),
      min_n  = enquo(min_n),
      class_cost = enquo(class_cost)
    )

    new_model_spec(
      "bag_tree",
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

#' @method update bag_tree
#' @rdname parsnip_update
#' @inheritParams decision_tree
#' @inheritParams bag_tree
#' @export
update.bag_tree <-
  function(object,
           parameters = NULL,
           cost_complexity = NULL, tree_depth = NULL, min_n = NULL,
           class_cost = NULL,
           fresh = FALSE, ...) {

    args <- list(
      cost_complexity = enquo(cost_complexity),
      tree_depth      = enquo(tree_depth),
      min_n           = enquo(min_n),
      class_cost      = enquo(class_cost)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "bag_tree",
      ...
    )
  }


# ------------------------------------------------------------------------------

#' @export
check_args.bag_tree <- function(object, call = rlang::caller_env()) {
  invisible(object)
}


# ------------------------------------------------------------------------------

set_new_model("bag_tree")
set_model_mode("bag_tree", "classification")
set_model_mode("bag_tree", "regression")
set_model_mode("bag_tree", "censored regression")
