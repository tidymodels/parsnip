# Prototype parsnip code for decision trees

#' Decision trees
#'
#' @description
#' `decision_tree()` creates a model that is defined as a set of `if/then`
#' statements that creates a tree-based structure.
#'
#' There are different ways to fit this model. Information about the available
#' _engines_ that can be used for fitting at:
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::find_engine_files("decision_tree")}
#'
#' More information on how `parsnip` is used for model is at
#' \url{https://www.tidymodels.org}.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param cost_complexity A positive number for the the cost/complexity
#'   parameter (a.k.a. `Cp`) used by CART models (specific engines only).
#' @param tree_depth An integer for maximum depth of the tree.
#' @param min_n An integer for the minimum number of data points
#'  in a node that are required for the node to be split further.
#' @details
#' This function only defines what _type_ of model is being fit. Once an engine
#'  is specified, the _method_ to fit the model is also defined.
#'
#' The model is not trained or fit until the [fit.model_spec()] function is used
#' with the data.
#'
#' @references \url{https://www.tidymodels.org},
#' [_Tidy Models with R_](https://tmwr.org)
#' @seealso [fit.model_spec()], [set_engine()], [update()],
#' \code{\link[=details_decision_tree_rpart]{rpart engine details}},
#' \code{\link[=details_decision_tree_C5.0]{C5.0 engine details}},
#' \code{\link[=details_decision_tree_spark]{spark engine details}}
#' @examples
#' show_engines("decision_tree")
#'
#' decision_tree(mode = "classification", tree_depth = 5)
#' @export

decision_tree <-
  function(mode = "unknown", cost_complexity = NULL, tree_depth = NULL, min_n = NULL) {

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
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.decision_tree <- function(x, ...) {
  cat("Decision Tree Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
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

    eng_args <- update_engine_parameters(object$eng_args, ...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }
    args <- list(
      cost_complexity   = enquo(cost_complexity),
      tree_depth  = enquo(tree_depth),
      min_n  = enquo(min_n)
    )

    args <- update_main_parameters(args, parameters)

    if (fresh) {
      object$args <- args
      object$eng_args <- eng_args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
      if (length(eng_args) > 0)
        object$eng_args[names(eng_args)] <- eng_args
    }

    new_model_spec(
      "decision_tree",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
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
      rlang::abort(
        glue::glue(
          "For spark decision tree models, the mode cannot be 'unknown' ",
          "if the specification is to be translated."
        )
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

check_args.decision_tree <- function(object) {
  if (object$engine == "C5.0" && object$mode == "regression")
    rlang::abort("C5.0 is classification only.")
  invisible(object)
}

# ------------------------------------------------------------------------------

#' Decision trees via rpart
#'
#' `rpart_train` is a wrapper for `rpart()` tree-based models
#'  where all of the model arguments are in the main function.
#'
#' @param formula A model formula.
#' @param data A data frame.
#' @param cp A non-negative number for complexity parameter. Any split
#'  that does not decrease the overall lack of fit by a factor of
#'  `cp` is not attempted. For instance, with anova splitting,
#'  this means that the overall R-squared must increase by `cp` at
#'  each step. The main role of this parameter is to save computing
#'  time by pruning off splits that are obviously not worthwhile.
#'  Essentially, the user informs the program that any split which
#'  does not improve the fit by `cp` will likely be pruned off by
#'  cross-validation, and that hence the program need not pursue it.
#' @param weights Optional case weights.
#' @param minsplit An integer for the minimum number of observations
#'  that must exist in a node in order for a split to be attempted.
#' @param maxdepth An integer for the maximum depth of any node
#'  of the final tree, with the root node counted as depth 0.
#'  Values greater than 30 `rpart` will give nonsense results on
#'  32-bit machines. This function will truncate `maxdepth` to 30 in
#'  those cases.
#' @param ... Other arguments to pass to either `rpart` or `rpart.control`.
#' @return A fitted rpart model.
#' @keywords internal
#' @export
rpart_train <-
  function(formula, data, weights = NULL, cp = 0.01, minsplit = 20, maxdepth = 30, ...) {
    bitness <- 8 * .Machine$sizeof.pointer
    if (bitness == 32 & maxdepth > 30)
      maxdepth <- 30

    other_args <- list(...)
    protect_ctrl <- c("minsplit", "maxdepth", "cp")
    protect_fit <- NULL
    f_names <- names(formals(getFromNamespace("rpart", "rpart")))
    c_names <- names(formals(getFromNamespace("rpart.control", "rpart")))
    other_args <- other_args[!(other_args %in% c(protect_ctrl, protect_fit))]
    ctrl_args <- other_args[names(other_args) %in% c_names]
    fit_args <- other_args[names(other_args) %in% f_names]

    ctrl <- call2("rpart.control", .ns = "rpart")
    ctrl$minsplit <- minsplit
    ctrl$maxdepth <- maxdepth
    ctrl$cp <- cp
    ctrl <- rlang::call_modify(ctrl, !!!ctrl_args)

    fit_call <- call2("rpart", .ns = "rpart")
    fit_call$formula <- expr(formula)
    fit_call$data <- expr(data)
    fit_call$control <- ctrl
    if (!is.null(weights)) {
      fit_call$weights <- quote(weights)
    }
    fit_call <- rlang::call_modify(fit_call, !!!fit_args)

    eval_tidy(fit_call)
  }

