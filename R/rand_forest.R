# Prototype parsnip code for random forests

#' General Interface for Random Forest Models
#'
#' `rand_forest` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R or via Spark. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{mtry}: The number of predictors that will be
#'   randomly sampled at each split when creating the tree models.
#'   \item \code{trees}: The number of trees contained in the ensemble.
#'   \item \code{min_n}: The minimum number of data points in a node
#'   that are required for the node to be split further.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using the `others` argument. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param others A named list of arguments to be used by the
#'  underlying models (e.g., `ranger::ranger`,
#'  `randomForest::randomForest`, etc.). .
#' @param mtry An integer for the number of predictors that will
#'  be randomly sampled at each split when creating the tree models.
#' @param trees An integer for the number of trees contained in
#'  the ensemble.
#' @param min_n An integer for the minimum number of data points
#'  in a node that are required for the node to be split further.
#' @param ... Used for method consistency. Any arguments passed to
#'  the ellipses will result in an error. Use `others` instead.
#' @details
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"ranger"` or `"randomForest"`
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
#' \pkg{ranger} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::rand_forest(mode = "classification"), "ranger")}
#'
#' \pkg{ranger} regression
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::rand_forest(mode = "regression"), "ranger")}
#'
#' \pkg{randomForests} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::rand_forest(mode = "classification"), "randomForest")}
#'
#' \pkg{randomForests} regression
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::rand_forest(mode = "regression"), "randomForest")}
#'
#' \pkg{spark} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::rand_forest(mode = "classification"), "spark")}
#'
#' \pkg{spark} regression
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::rand_forest(mode = "regression"), "spark")}
#'
#' For \pkg{ranger} confidence intervals, the intervals are
#'  constructed using the form `estimate +/- z * std_error`. For
#'  classification probabilities, these values can fall outside of
#'  `[0, 1]` and will be coerced to be in this range.
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
#' rand_forest(mode = "classification", trees = 2000)
#' # Parameters can be represented by a placeholder:
#' rand_forest(mode = "regression", mtry = varying())
#' @export

rand_forest <-
  function(mode = "unknown",
           mtry = NULL, trees = NULL, min_n = NULL,
           others = list(),
           ...) {
    check_empty_ellipse(...)

    ## TODO: make a utility function here
    if (!(mode %in% rand_forest_modes))
      stop("`mode` should be one of: ",
           paste0("'", rand_forest_modes, "'", collapse = ", "),
           call. = FALSE)

    args <- list(mtry = mtry, trees = trees, min_n = min_n)

    no_value <- !vapply(others, is.null, logical(1))
    others <- others[no_value]

    # write a constructor function
    out <- list(args = args, others = others,
                mode = mode, method = NULL, engine = NULL)
    # TODO: make_classes has wrong order; go from specific to general
    class(out) <- make_classes("rand_forest")
    out
  }

#' @export
print.rand_forest <- function(x, ...) {
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
#' @inheritParams rand_forest
#' @param object A random forest model specification.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @return An updated model specification.
#' @examples
#' model <- rand_forest(mtry = 10, min_n = 3)
#' model
#' update(model, mtry = 1)
#' update(model, mtry = 1, fresh = TRUE)
#' @method update rand_forest
#' @rdname rand_forest
#' @export
update.rand_forest <-
  function(object,
           mtry = NULL, trees = NULL, min_n = NULL,
           others = list(),
           fresh = FALSE,
           ...) {
    check_empty_ellipse(...)

    args <- list(mtry = mtry, trees = trees, min_n = min_n)

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

# ------------------------------------------------------------------------------

#' @export
translate.rand_forest <- function(x, engine, ...) {
  x <- translate.default(x, engine, ...)

  if (x$engine == "spark") {
    if (x$mode == "unknown")
      stop(
        "For spark random forests models, the mode cannot be 'unknown' ",
        "if the specification is to be translated.",
        call. = FALSE
      )
    else
      x$method$fit$args$type <- x$mode

    # See "Details" in ?ml_random_forest_classifier
    if (is.numeric(x$method$fit$args$feature_subset_strategy))
      x$method$fit$args$feature_subset_strategy <-
        paste(x$method$fit$args$feature_subset_strategy)

  }

  # add checks to error trap or change things for this method
  if (x$engine == "ranger") {
    if (any(names(x$method$fit$args) == "importance"))
      if (is.logical(x$method$fit$args$importance))
        stop("`importance` should be a character value. See ?ranger::ranger.",
             call. = FALSE)
    # unless otherwise specified, classification models are probability forests
    if (x$mode == "classification" && !any(names(x$method$fit$args) == "probability"))
      x$method$fit$args$probability <- TRUE

  }
  x
}

