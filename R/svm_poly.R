#' General Interface for polynomial support vector machines
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
#'  set using the  `...` slot. If left to their defaults
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
#'  model fit call. These can be changed by using the `...`
#'  argument to pass in the preferred values. For this type of
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
           cost = NULL, degree = NULL, scale_factor = NULL, margin = NULL, ...) {
    
    others <- enquos(...)
    
    args <- list(
      cost   = enquo(cost),
      degree  = enquo(degree),
      scale_factor  = enquo(scale_factor),
      margin = enquo(margin)
    )
    
    if (!(mode %in% svm_poly_modes))
      stop("`mode` should be one of: ",
           paste0("'", svm_poly_modes, "'", collapse = ", "),
           call. = FALSE)
    
    no_value <- !vapply(others, null_value, logical(1))
    others <- others[no_value]
    
    # write a constructor function
    out <- list(args = args, others = others,
                mode = mode, method = NULL, engine = NULL)
    class(out) <- make_classes("svm_poly")
    out
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
    others <- enquos(...)
    
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
translate.svm_poly <- function(x, engine, ...) {
  x <- translate.default(x, engine, ...)
  
  # slightly cleaner code using
  arg_vals <- x$method$fit$args
  
  # add checks to error trap or change things for this method
  if (x$engine == "kernlab") {
    
    # unless otherwise specified, classification models predict probabilities
    if (x$mode == "classification" && !any(names(arg_vals) == "prob.model"))
      arg_vals$prob.model <- TRUE
    
  }
  x$method$fit$args <- arg_vals
  
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

