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
#'  functions.
#' 
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `rand_forest`, the
#'  possible modes are "regression" and "classification".
#' 
#' The model can be created using the [fit()] function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"ranger"` or `"randomForests"` 
#' \item \pkg{Spark}: `"spark"`
#' }
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
#' @details Main parameter arguments (and those in `others`) can avoid 
#'  evaluation until the underlying function is executed by wrapping the 
#'  argument in [rlang::expr()] (e.g. `mtry = expr(floor(sqrt(p)))`).
#' @importFrom purrr map_lgl
#' @seealso [varying()], [fit()]
#' @examples 
#' rand_forest(mode = "classification", trees = 2000)
#' 
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
  invisible(x)
}

###################################################################

#' Update a Random Forest Specification
#' 
#' If parameters need to be modified, this function can be used
#'  in lieu of recreating the object from scratch. 
#'  
#' @export
#' @inheritParams rand_forest
#' @param object A random forest model specification. 
#' @param fresh A logical for whether the arguments should be
#'  modifed in-place of or replaced wholesale. 
#' @return An updated model specification.
#' @examples 
#' model <- rand_forest(mtry = 10, min_n = 3)
#' model
#' 
#' update(model, mtry = 1)
#' 
#' update(model, mtry = 1, fresh = TRUE)
#' @method update rand_forest
#' @export
update.rand_forest <-
  function(object,
           mtry = NULL, trees = NULL, min_n = NULL,
           others = list(),
           fresh = FALSE,
           ...) {
    check_empty_ellipse(...)
    
    args <- list(mtry = mtry, trees = trees, min_n = min_n)
    
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

finalize.rand_forest <- function(x, engine, ...) {
  x <- finalize.default(x, engine, ...)
  
  # add checks to error trap or change things for this method
  if (engine == "ranger") {
    if (any(names(x$others) == "importance"))
      if (is.logical(x$others$importance))
        stop("`importance` should be a character value. See ?ranger::ranger.",
             call. = FALSE)
  }
  x
}

## TODO make a class-specific constructor that customizes the rf syntax + checks