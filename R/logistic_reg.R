#' General Interface for Logistic Regression Models
#' 
#' `logistic_reg` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R, Stan, or via Spark. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{regularization}: The total amount of regularization
#'  in the model. Note that this must be zero for some engines.
#'   \item \code{mixture}: The proportion of L2 regularization in
#'  the model. Note that this will be ignored for some engines.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using the `others` argument. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions.
#' 
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `logistic_reg`,the
#'  mode will always be "classification".
#' 
#' The model can be created using the [fit()] function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"glm"` or `"glmnet"` 
#' \item \pkg{Stan}:  `"rstanarm"` 
#' \item \pkg{Spark}: `"spark"`
#' }
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "classification".
#' @param others A named list of arguments to be used by the
#'  underlying models (e.g., `stats::glm`,
#'  `rstanarm::stan_glm`, etc.). These are not evaluated
#'  until the model is fit and will be substituted into the model
#'  fit expression.
#' @param regularization An non-negative number representing the
#'  total amount of regularization.
#' @param mixture A number between zero and one (inclusive) that
#'  represents the proportion of regularization that is used for the
#'  L2 penalty (i.e. weight decay, or ridge regression) versus L1
#'  (the lasso).
#' @param ... Used for S3 method consistency. Any arguments passed to
#'  the ellipses will result in an error. Use `others` instead.
#' @seealso [varying()], [fit()]
#' @examples 
#' logistic_reg()
#' 
#' # Parameters can be represented by a placeholder:
#' logistic_reg(regularization = varying())
#' @export
#' @importFrom purrr map_lgl
logistic_reg <-
  function(mode = "classification",
           regularization = NULL,
           mixture = NULL,
           others = list(),
           ...) {
    check_empty_ellipse(...)
    if (!(mode %in% logistic_reg_modes))
      stop(
        "`mode` should be one of: ",
        paste0("'", logistic_reg_modes, "'", collapse = ", "),
        call. = FALSE
      )
    
    args <- list(
      regularization = rlang::enquo(regularization),
      mixture = rlang::enquo(mixture)
    )
    
    others <- parse_engine_options(rlang::enquo(others))
    
    # write a constructor function
    out <- list(
      args = args,
      others = others,
      mode = mode,
      method = NULL,
      engine = NULL
    )
    class(out) <- make_classes("logistic_reg")
    out
  }

#' @export
print.logistic_reg <- function(x, ...) {
  cat("Logistic Regression Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)
  invisible(x)
}

###################################################################

#' Update a Logistic Regression Specification
#' 
#' If parameters need to be modified, this function can be used
#'  in lieu of recreating the object from scratch. 
#'  
#' @inheritParams logistic_reg
#' @param object A logistic reression model specification. 
#' @param fresh A logical for whether the arguments should be
#'  modifed in-place of or replaced wholesale. 
#' @return An updated model specification.
#' @examples 
#' model <- logistic_reg(regularization = 10, mixture = 0.1)
#' model
#' 
#' update(model, regularization = 1)
#' 
#' update(model, regularization = 1, fresh = TRUE)
#' @method update logistic_reg
#' @export
update.logistic_reg <-
  function(object,
           regularization = NULL, mixture = NULL,
           others = list(),
           fresh = FALSE,
           ...) {
    check_empty_ellipse(...)
    
    args <- list(
      regularization = rlang::enquo(regularization),
      mixture = rlang::enquo(mixture)
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


###################################################################

logistic_reg_arg_key <- data.frame(
  glm    =  c(        NA,                  NA),
  glmnet =  c(   "lambda",             "alpha"),
  spark  =  c("reg_param", "elastic_net_param"),
  stan   =  c(        NA,                  NA),
  stringsAsFactors = FALSE,
  row.names =  c("regularization", "mixture")
)

logistic_reg_modes <- "classification"

logistic_reg_engines <- data.frame(
  glm    = TRUE,
  glmnet = TRUE,
  spark  = TRUE,
  stan   = TRUE,  
  row.names =  c("classification")
)

