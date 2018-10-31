# Prototype parsnip code for decision trees

#' General Interface for Decision Tree Models
#'
#' `decision_tree` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R or via Spark. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{cost_complexity}: The cost/complexity parameter (a.k.a. `Cp`)
#'    used by CART models (`rpart` only).
#'   \item \code{tree_depth}: The _maximum_ depth of a tree (`rpart` and
#'   `spark` only).
#'   \item \code{min_n}: The minimum number of data points in a node
#'   that are required for the node to be split further.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using `set_engine`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @inheritParams boost_tree
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param cost_complexity A positive number for the the cost/complexity
#'   parameter (a.k.a. `Cp`) used by CART models (`rpart` only).
#' @param tree_depth An integer for maximum depth of the tree.
#' @param min_n An integer for the minimum number of data points
#'  in a node that are required for the node to be split further.
#' @details
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"rpart"` or `"C5.0"` (classification only)
#' \item \pkg{Spark}: `"spark"`
#' }
#'
#' Note that, for `rpart` models, but `cost_complexity` and
#'  `tree_depth` can be both be specified but the package will give
#'  precedence to `cost_complexity`. Also, `tree_depth` values
#'  greater than 30 `rpart` will give nonsense results on 32-bit
#'  machines.
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call. For this type of
#'  model, the template of the fit calls are::
#'
#' \pkg{rpart} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::decision_tree(mode = "classification"), "rpart")}
#'
#' \pkg{rpart} regression
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::decision_tree(mode = "regression"), "rpart")}
#'
#' \pkg{C5.0} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::decision_tree(mode = "classification"), "C5.0")}
#'
#' \pkg{spark} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::decision_tree(mode = "classification"), "spark")}
#'
#' \pkg{spark} regression
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::decision_tree(mode = "regression"), "spark")}
#'
#' @note For models created using the spark engine, there are
#'  several differences to consider. First, only the formula
#'  interface to via `fit` is available; using `fit_xy` will
#'  generate an error. Second, the predictions will always be in a
#'  spark table format. The names will be the same as documented but
#'  without the dots. Third, there is no equivalent to factor
#'  columns in spark tables so class predictions are returned as
#'  character columns. Fourth, to retain the model object for a new
#'  R session (via `save`), the `model$fit` element of the `parsnip`
#'  object should be serialized via `ml_save(object$fit)` and
#'  separately saved to disk. In a new session, the object can be
#'  reloaded and reattached to the `parsnip` object.
#'
#' @importFrom purrr map_lgl
#' @seealso [varying()], [fit()]
#' @examples
#' decision_tree(mode = "classification", tree_depth = 5)
#' # Parameters can be represented by a placeholder:
#' decision_tree(mode = "regression", cost_complexity = varying())
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
  cat("Random Forest Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @export
#' @inheritParams update.boost_tree
#' @param object A random forest model specification.
#' @examples
#' model <- decision_tree(cost_complexity = 10, min_n = 3)
#' model
#' update(model, cost_complexity = 1)
#' update(model, cost_complexity = 1, fresh = TRUE)
#' @method update decision_tree
#' @rdname decision_tree
#' @export
update.decision_tree <-
  function(object,
           cost_complexity = NULL, tree_depth = NULL, min_n = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)
    args <- list(
      cost_complexity   = enquo(cost_complexity),
      tree_depth  = enquo(tree_depth),
      min_n  = enquo(min_n)
    )

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
    message("Used `engine = 'ranger'` for translation.")
    engine <- "ranger"
  }

  x <- translate.default(x, engine, ...)

  # slightly cleaner code using
  arg_vals <- x$method$fit$args

  if (x$engine == "spark") {
    if (x$mode == "unknown") {
      stop(
        "For spark random forests models, the mode cannot be 'unknown' ",
        "if the specification is to be translated.",
        call. = FALSE
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
    if (any(names(arg_vals) == "importance"))
      if (isTRUE(is.logical(quo_get_expr(arg_vals$importance))))
        stop("`importance` should be a character value. See ?ranger::ranger.",
             call. = FALSE)
    # unless otherwise specified, classification models are probability forests
    if (x$mode == "classification" && !any(names(arg_vals) == "probability"))
      arg_vals$probability <- TRUE

  }
  x$method$fit$args <- arg_vals

  x
}

# ------------------------------------------------------------------------------

check_args.decision_tree <- function(object) {
  if (object$engine == "C5.0" && object$mode == "regression")
    stop("C5.0 is classification only.", call. = FALSE)
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
#'  Essentially,the user informs the program that any split which
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
    for(i in names(ctrl_args))
      ctrl[[i]] <- ctrl_args[[i]]

    fit_call <- call2("rpart", .ns = "rpart")
    fit_call$formula <- expr(formula)
    fit_call$data <- expr(data)
    fit_call$control <- ctrl
    if(!is.null(weights))
      fit_call$weights <- quote(weights)

    for(i in names(fit_args))
      fit_call[[i]] <- fit_args[[i]]

    eval_tidy(fit_call)
  }

