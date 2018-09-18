# Prototype parsnip code for boosted trees

#' General Interface for Boosted Trees
#'
#' `boost_tree` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R or via Spark. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{mtry}: The number of predictors that will be
#'   randomly sampled at each split when creating the tree models.
#'   \item \code{trees}: The number of trees contained in the ensemble.
#'   \item \code{min_n}: The minimum number of data points in a node
#'   that are required for the node to be split further.
#'   \item \code{tree_depth}: The maximum depth of the tree (i.e. number of
#'  splits).
#'   \item \code{learn_rate}: The rate at which the boosting algorithm adapts
#'   from iteration-to-iteration.
#'   \item \code{loss_reduction}: The reduction in the loss function required
#'   to split further.
#'   \item \code{sample_size}: The amount of data exposed to the fitting routine.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using the `others` argument. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions.  If parameters need to be modified, `update` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param others A named list of arguments to be used by the
#'  underlying models (e.g., `xgboost::xgb.train`, etc.). .
#' @param mtry An number for the number (or proportion) of predictors that will
#'  be randomly sampled at each split when creating the tree models (`xgboost`
#'  only).
#' @param trees An integer for the number of trees contained in
#'  the ensemble.
#' @param min_n An integer for the minimum number of data points
#'  in a node that are required for the node to be split further.
#' @param tree_depth An integer for the maximum deopth of the tree (i.e. number
#'  of splits) (`xgboost` only).
#' @param learn_rate A number for the rate at which the boosting algorithm adapts
#'   from iteration-to-iteration (`xgboost` only).
#' @param loss_reduction A number for the reduction in the loss function required
#'   to split further  (`xgboost` only).
#' @param sample_size An number for the number (or proportion) of data that is
#'  exposed to the fitting routine. For `xgboost`, the sampling is done at at
#'  each iteration while `C5.0` samples once during traning.
#' @param ... Used for method consistency. Any arguments passed to
#'  the ellipses will result in an error. Use `others` instead.
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `boost_tree`, the
#'  possible modes are "regression" and "classification".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"xgboost"`, `"C5.0"`
#' \item \pkg{Spark}: `"spark"`
#' }
#'
#' Main parameter arguments (and those in `others`) can avoid
#'  evaluation until the underlying function is executed by wrapping the
#'  argument in [rlang::expr()] (e.g. `mtry = expr(floor(sqrt(p)))`).
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call. These can be changed by using the `others`
#'  argument to pass in the preferred values. For this type of
#'  model, the template of the fit calls are:
#'
#' \pkg{xgboost} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::boost_tree(mode = "classification"), "xgboost")}
#'
#' \pkg{xgboost} regression
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::boost_tree(mode = "regression"), "xgboost")}
#'
#' \pkg{C5.0} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::boost_tree(mode = "classification"), "C5.0")}
#'
#' @importFrom purrr map_lgl
#' @seealso [varying()], [fit()]
#' @examples
#' boost_tree(mode = "classification", trees = 20)
#' # Parameters can be represented by a placeholder:
#' boost_tree(mode = "regression", mtry = varying())
#' @export

boost_tree <-
  function(mode = "unknown",
           mtry = NULL, trees = NULL, min_n = NULL,
           tree_depth = NULL, learn_rate = NULL,
           loss_reduction = NULL,
           sample_size = NULL,
           others = list(),
           ...) {
    check_empty_ellipse(...)

    if (!(mode %in% boost_tree_modes))
      stop("`mode` should be one of: ",
           paste0("'", boost_tree_modes, "'", collapse = ", "),
           call. = FALSE)

    if (is.numeric(trees) && trees < 0)
      stop("`trees` should be >= 1", call. = FALSE)
    if (is.numeric(sample_size) && (sample_size < 0 | sample_size > 1))
      stop("`sample_size` should be within [0,1]", call. = FALSE)
    if (is.numeric(tree_depth) && tree_depth < 0)
      stop("`tree_depth` should be >= 1", call. = FALSE)
    if (is.numeric(min_n) && min_n < 0)
      stop("`min_n` should be >= 1", call. = FALSE)

    args <- list(
      mtry = mtry, trees = trees, min_n = min_n, tree_depth = tree_depth,
      learn_rate = learn_rate, loss_reduction = loss_reduction,
      sample_size = sample_size
    )

    no_value <- !vapply(others, is.null, logical(1))
    others <- others[no_value]

    out <- list(args = args, others = others,
                mode = mode, method = NULL, engine = NULL)
    class(out) <- make_classes("boost_tree")
    out
  }

#' @export
print.boost_tree <- function(x, ...) {
  cat("Boosted Tree Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

###################################################################

#' @export
#' @inheritParams boost_tree
#' @param object A boosted tree model specification.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @return An updated model specification.
#' @examples
#' model <- boost_tree(mtry = 10, min_n = 3)
#' model
#' update(model, mtry = 1)
#' update(model, mtry = 1, fresh = TRUE)
#' @method update boost_tree
#' @rdname boost_tree
#' @export
update.boost_tree <-
  function(object,
           mtry = NULL, trees = NULL, min_n = NULL,
           tree_depth = NULL, learn_rate = NULL,
           loss_reduction = NULL, sample_size = NULL,
           others = list(),
           fresh = FALSE,
           ...) {
    check_empty_ellipse(...)

    args <- list(
      mtry = mtry, trees = trees, min_n = min_n, tree_depth = tree_depth,
      learn_rate = learn_rate, loss_reduction = loss_reduction,
      sample_size = sample_size
      )

    # TODO make these blocks into a function and document well
    if (fresh) {
      object$args <- args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }

    if (length(others) > 0) {
      if (fresh)
        object$others <- others
      else
        object$others[names(others)] <- others
    }

    object
  }

###################################################################

#' @export
translate.boost_tree <- function(x, engine, ...) {
  x <- translate.default(x, engine, ...)

  if (x$engine == "spark") {
    if (x$mode == "unknown")
      stop(
        "For spark boosted trees models, the mode cannot be 'unknown' ",
        "if the specification is to be translated.",
        call. = FALSE
      )
    else
      x$method$fit$args$type <- x$mode
  }
  x
}
