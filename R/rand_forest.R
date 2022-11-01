#' Random forest
#'
#' @description
#'
#' `rand_forest()` defines a model that creates a large number of decision
#' trees, each independent of the others. The final prediction uses all
#' predictions from the individual trees and combines them. This function can fit
#' classification, regression, and censored regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("rand_forest")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @param mtry An integer for the number of predictors that will
#'  be randomly sampled at each split when creating the tree models.
#' @param trees An integer for the number of trees contained in
#'  the ensemble.
#' @param min_n An integer for the minimum number of data points
#'  in a node that are required for the node to be split further.
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("rand_forest")}
#'
#' @examples
#' show_engines("rand_forest")
#'
#' rand_forest(mode = "classification", trees = 2000)
#' @export

rand_forest <-
  function(mode = "unknown", engine = "ranger", mtry = NULL, trees = NULL, min_n = NULL) {

    args <- list(
      mtry   = enquo(mtry),
      trees  = enquo(trees),
      min_n  = enquo(min_n)
    )

    new_model_spec(
      "rand_forest",
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

#' @method update rand_forest
#' @rdname parsnip_update
#' @export
update.rand_forest <-
  function(object,
           parameters = NULL,
           mtry = NULL, trees = NULL, min_n = NULL,
           fresh = FALSE, ...) {

    args <- list(
      mtry   = enquo(mtry),
      trees  = enquo(trees),
      min_n  = enquo(min_n)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "rand_forest",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
translate.rand_forest <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'ranger'` for translation.")
    engine <- "ranger"
  }

  x <- translate.default(x, engine, ...)

  ## -----------------------------------------------------------------------------

  # slightly cleaner code using
  arg_vals <- x$method$fit$args

  if (x$engine == "spark") {
    if (x$mode == "unknown") {
      rlang::abort(
        glue::glue("For spark random forests models, the mode cannot ",
                   "be 'unknown' if the specification is to be translated.")
      )
    } else {
      arg_vals$type <- x$mode
    }

    # See "Details" in ?ml_random_forest_classifier. `feature_subset_strategy`
    # should be character even if it contains a number.
    if (any(names(arg_vals) == "feature_subset_strategy") &&
        isTRUE(is.numeric(quo_get_expr(arg_vals$feature_subset_strategy)))) {
      arg_vals$feature_subset_strategy <-
        paste(quo_get_expr(arg_vals$feature_subset_strategy))
    }
  }

  # add checks to error trap or change things for this method
  if (engine == "ranger") {

    if (any(names(arg_vals) == "importance")) {
      if (isTRUE(is.logical(quo_get_expr(arg_vals$importance)))) {
        rlang::abort("`importance` should be a character value. See ?ranger::ranger.")
      }
    }
    # unless otherwise specified, classification models are probability forests
    if (x$mode == "classification" && !any(names(arg_vals) == "probability")) {
      arg_vals$probability <- TRUE
    }
  }

  ## -----------------------------------------------------------------------------
  # Protect some arguments based on data dimensions

  if (any(names(arg_vals) == "mtry") & engine != "partykit") {
    arg_vals$mtry <- rlang::call2("min_cols", arg_vals$mtry, expr(x))
  }

  if (any(names(arg_vals) == "min.node.size")) {
    arg_vals$min.node.size <-
      rlang::call2("min_rows", arg_vals$min.node.size, expr(x))
  }

  if (any(names(arg_vals) == "nodesize")) {
    arg_vals$nodesize <-
      rlang::call2("min_rows", arg_vals$nodesize, expr(x))
  }
  if (any(names(arg_vals) == "min_instances_per_node")) {
    arg_vals$min_instances_per_node <-
      rlang::call2("min_rows", arg_vals$min_instances_per_node, expr(x))
  }

  ## -----------------------------------------------------------------------------

  x$method$fit$args <- arg_vals

  x
}

# ------------------------------------------------------------------------------

check_args.rand_forest <- function(object) {
  # move translate checks here?
  invisible(object)
}

# ------------------------------------------------------------------------------

#' @export
fit_xy.rand_forest <- function(object,
                               x,
                               y,
                               case_weights = NULL,
                               control = parsnip::control_parsnip(),
                               ...) {

  if (object$mode == "censored regression" && object$engine == "aorsf") {
    # aorsf::orsf() requires two variables on the left-hand side of the formula,
    # either in as `Surv(time, status) ~ .` or as `time + status ~ .`
    rlang::abort("For the `'aorsf'` engine, please use the formula interface via `fit()`.")
  }

  # call parsnip::fit_xy.model_spec()
  res <- NextMethod()
  res
}
