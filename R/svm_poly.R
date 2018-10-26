#' General interface for polynomial support vector machines
#'
#' `svm_poly` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R or via Spark. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{cost}: The cost of predicting a sample within or on the 
#'    wrong side of the margin. 
#'   \item \code{degree}: The polynomial degree.
#'   \item \code{scale_factor}: A scaling factor for the kernel.
#'   \item \code{margin}: The epsilon in the SVM insensitive loss function 
#'    (regression only)
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
#' @param cost A positive number for the cost of predicting a sample within 
#'  or on the wrong side of the margin
#' @param degree A positive number for polynomial degree.
#' @param scale_factor A positive number for the polynomial scaling factor. 
#' @param margin A positive number for the epsilon in the SVM insensitive 
#'   loss function (regression only)
#' @details
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"kernlab"` 
#' }
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call. For this type of
#'  model, the template of the fit calls are::
#'
#' \pkg{kernlab} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::svm_poly(mode = "classification"), "kernlab")}
#'
#' \pkg{kernlab} regression
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::svm_poly(mode = "regression"), "kernlab")}
#'
#' @importFrom purrr map_lgl
#' @seealso [varying()], [fit()]
#' @examples
#' svm_poly(mode = "classification", degree = 1.2)
#' # Parameters can be represented by a placeholder:
#' svm_poly(mode = "regression", cost = varying())
#' @export

svm_poly <-
  function(mode = "unknown",
           cost = NULL, degree = NULL, scale_factor = NULL, margin = NULL) {
    
    args <- list(
      cost   = enquo(cost),
      degree  = enquo(degree),
      scale_factor  = enquo(scale_factor),
      margin = enquo(margin)
    )
    
    new_model_spec(
      "svm_poly",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.svm_poly <- function(x, ...) {
  cat("Polynomial Support Vector Machine Specification (", x$mode, ")\n\n", sep = "")
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
#' @param object A polynomial SVM model specification.
#' @examples
#' model <- svm_poly(cost = 10, scale_factor = 0.1)
#' model
#' update(model, cost = 1)
#' update(model, cost = 1, fresh = TRUE)
#' @method update svm_poly
#' @rdname svm_poly
#' @export
update.svm_poly <-
  function(object,
           cost = NULL, degree = NULL, scale_factor = NULL, margin = NULL,
           fresh = FALSE,
           ...) {
    update_dot_check(...)
    
    args <- list(
      cost   = enquo(cost),
      degree  = enquo(degree),
      scale_factor  = enquo(scale_factor),
      margin  = enquo(margin)
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
      "svm_poly",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

#' @export
translate.svm_poly <- function(x, engine = x$engine, ...) {
  x <- translate.default(x, engine = engine, ...)
  
  # slightly cleaner code using
  arg_vals <- x$method$fit$args
  arg_names <- names(arg_vals)
  
  # add checks to error trap or change things for this method
  if (x$engine == "kernlab") {
    
    # unless otherwise specified, classification models predict probabilities
    if (x$mode == "classification" && !any(arg_names == "prob.model"))
      arg_vals$prob.model <- TRUE
    if (x$mode == "classification" && any(arg_names == "epsilon"))
      arg_vals$epsilon <- NULL
    
    # convert degree and scale to a `kpar` argument.
    if (any(arg_names %in% c("degree", "scale", "offset"))) {
      kpar <- expr(list())
      if (any(arg_names == "degree")) {
        kpar$degree <- arg_vals$degree
        arg_vals$degree <- NULL
      }
      if (any(arg_names == "scale")) {
        kpar$scale <- arg_vals$scale
        arg_vals$scale <- NULL
      }    
      if (any(arg_names == "offset")) {
        kpar$offset <- arg_vals$offset
        arg_vals$offset <- NULL
      } 
      arg_vals$kpar <- kpar
    }
    
  }
  x$method$fit$args <- arg_vals
  
  # worried about people using this to modify the specification
  x
}

# ------------------------------------------------------------------------------

check_args.svm_poly <- function(object) {
  invisible(object)
}

# ------------------------------------------------------------------------------

svm_reg_post <- function(results, object) { 
  results[,1]
}

