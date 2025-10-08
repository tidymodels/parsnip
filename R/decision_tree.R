# Prototype parsnip code for decision trees

#' Decision trees
#'
#' @description
#' `decision_tree()` defines a model as a set of `if/then` statements that
#' creates a tree-based structure. This function can fit classification,
#' regression, and censored regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("decision_tree")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @param cost_complexity A positive number for the the cost/complexity
#'   parameter (a.k.a. `Cp`) used by CART models (specific engines only).
#' @param tree_depth An integer for maximum depth of the tree.
#' @param min_n An integer for the minimum number of data points
#'  in a node that are required for the node to be split further.
#'
#' @templateVar modeltype decision_tree
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("decision_tree")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("decision_tree")
#'
#' decision_tree(mode = "classification", tree_depth = 5)
#' @export

decision_tree <-
  function(mode = "unknown", engine = "rpart", cost_complexity = NULL,
           tree_depth = NULL, min_n = NULL) {

    args <- list(
      cost_complexity   = enquo(cost_complexity),
      tree_depth  = enquo(tree_depth),
      min_n  = enquo(min_n)
    )

    new_model_spec(
      "decision_tree",
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

#' @method update decision_tree
#' @rdname parsnip_update
#' @export
update.decision_tree <-
  function(object,
           parameters = NULL,
           cost_complexity = NULL, tree_depth = NULL, min_n = NULL,
           fresh = FALSE, ...) {

    args <- list(
      cost_complexity   = enquo(cost_complexity),
      tree_depth  = enquo(tree_depth),
      min_n  = enquo(min_n)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "decision_tree",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
translate.decision_tree <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'rpart'` for translation.")
    engine <- "rpart"
  }

  x <- translate.default(x, engine, ...)

  # slightly cleaner code using
  arg_vals <- x$method$fit$args

  if (x$engine == "spark") {
    if (x$mode == "unknown") {
      cli::cli_abort(
        "For spark decision tree models, the mode cannot be {.val unknown}
         if the specification is to be translated."
      )
    }
  }

  ## -----------------------------------------------------------------------------
  # Protect some arguments based on data dimensions

  if (any(names(arg_vals) == "minsplit")) {
    arg_vals$minsplit <-
      rlang::call2("min_rows", rlang::eval_tidy(arg_vals$minsplit), expr(data))
  }
  if (any(names(arg_vals) == "min_instances_per_node")) {
    arg_vals$min_instances_per_node <-
      rlang::call2("min_rows", rlang::eval_tidy(arg_vals$min_instances_per_node), expr(x))
  }

  ## -----------------------------------------------------------------------------

  x$method$fit$args <- arg_vals

  x
}

# ------------------------------------------------------------------------------

#' @export
check_args.decision_tree <- function(object, call = rlang::caller_env()) {
  invisible(object)
}
