# TODO) If implementing `class::knn()`, mention that it does not have
# the distance param because it uses Euclidean distance. And no `weight_func`
# param.

#' General Interface for K-Nearest Neighbor Models
#'
#' `nearest_neighbor()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{neighbors}: The number of neighbors considered at
#'   each prediction.
#'   \item \code{weight_func}: The type of kernel function that weights the
#'   distances between samples.
#'   \item \code{dist_power}: The parameter used when calculating the Minkowski
#'   distance. This corresponds to the Manhattan distance with `dist_power = 1`
#'   and the Euclidean distance with `dist_power = 2`.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using `set_engine`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#' @inheritParams boost_tree
#' @param mode A single character string for the type of model.
#' Possible values for this model are `"unknown"`, `"regression"`, or
#' `"classification"`.
#'
#' @param neighbors A single integer for the number of neighbors
#' to consider (often called `k`).
#'
#' @param weight_func A *single* character for the type of kernel function used
#' to weight distances between samples. Valid choices are: `"rectangular"`,
#' `"triangular"`, `"epanechnikov"`, `"biweight"`, `"triweight"`,
#' `"cos"`, `"inv"`, `"gaussian"`, `"rank"`, or `"optimal"`.
#'
#' @param dist_power A single number for the parameter used in
#' calculating Minkowski distance.
#'
#' @details
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"kknn"`
#' }
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call. For this type of
#'  model, the template of the fit calls are:
#'
#' \pkg{kknn} (classification or regression)
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::nearest_neighbor(), "kknn")}
#'
#' @note
#' For `kknn`, the underlying modeling function used is a restricted
#' version of `train.kknn()` and not `kknn()`. It is set up in this way so that
#' `parsnip` can utilize the underlying `predict.train.kknn` method to predict
#' on new data. This also means that a single value of that function's
#' `kernel` argument (a.k.a `weight_func` here) can be supplied
#'
#' @seealso [varying()], [fit()]
#'
#' @examples
#' nearest_neighbor(neighbors = 11)
#'
#' @export
nearest_neighbor <- function(mode = "unknown",
                             neighbors = NULL,
                             weight_func = NULL,
                             dist_power = NULL) {
  args <- list(
    neighbors   = enquo(neighbors),
    weight_func = enquo(weight_func),
    dist_power  = enquo(dist_power)
  )

  new_model_spec(
    "nearest_neighbor",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

#' @export
print.nearest_neighbor <- function(x, ...) {
  cat("K-Nearest Neighbor Model Specification (", x$mode, ")\n\n", sep = "")
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
update.nearest_neighbor <- function(object,
                                    neighbors = NULL,
                                    weight_func = NULL,
                                    dist_power = NULL,
                                    fresh = FALSE, ...) {
  update_dot_check(...)
  args <- list(
    neighbors   = enquo(neighbors),
    weight_func = enquo(weight_func),
    dist_power  = enquo(dist_power)
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
    "nearest_neighbor",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}


positive_int_scalar <- function(x) {
  (length(x) == 1) && (x > 0) && (x %% 1 == 0)
}

# ------------------------------------------------------------------------------

check_args.nearest_neighbor <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if(is.numeric(args$neighbors) && !positive_int_scalar(args$neighbors)) {
    stop("`neighbors` must be a length 1 positive integer.", call. = FALSE)
  }

  if(is.character(args$weight_func) && length(args$weight_func) > 1) {
    stop("The length of `weight_func` must be 1.", call. = FALSE)
  }

  invisible(object)
}
