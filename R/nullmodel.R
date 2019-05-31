#' Fit a simple, non-informative model
#'
#' Fit a single mean or largest class model. `nullmodel()` is the underlying
#' computational function for the `null_model()` specification.
#'
#' `nullmodel()` emulates other model building functions, but returns the
#' simplest model possible given a training set: a single mean for numeric
#' outcomes and the most prevalent class for factor outcomes. When class
#' probabilities are requested, the percentage of the training set samples with
#' the most prevalent class is returned.
#'
#' @aliases nullmodel nullmodel.default predict.nullmodel
#' @param x An optional matrix or data frame of predictors. These values are
#' not used in the model fit
#' @param y A numeric vector (for regression) or factor (for classification) of
#' outcomes
#' @param \dots Optional arguments (not yet used)
#' @param object An object of class \code{nullmodel}
#' @param new_data A matrix or data frame of predictors (only used to determine
#' the number of predictions to return)
#' @param type Either "raw" (for regression), "class" or "prob" (for
#' classification)
#' @return The output of `nullmodel()` is a list of class \code{nullmodel}
#' with elements \item{call }{the function call} \item{value }{the mean of
#' \code{y} or the most prevalent class} \item{levels }{when \code{y} is a
#' factor, a vector of levels. \code{NULL} otherwise} \item{pct }{when \code{y}
#' is a factor, a data frame with a column for each class (\code{NULL}
#' otherwise). The column for the most prevalent class has the proportion of
#' the training samples with that class (the other columns are zero). } \item{n
#' }{the number of elements in \code{y}}
#'
#' `predict.nullmodel()` returns a either a factor or numeric vector
#' depending on the class of \code{y}. All predictions are always the same.
#' @keywords models
#' @examples
#'
#' outcome <- factor(sample(letters[1:2],
#'                          size = 100,
#'                          prob = c(.1, .9),
#'                          replace = TRUE))
#' useless <- nullmodel(y = outcome)
#' useless
#' predict(useless, matrix(NA, nrow = 5))
#'
#' @export
nullmodel <- function (x, ...) UseMethod("nullmodel")

#' @export
#' @rdname nullmodel
nullmodel.default <- function(x = NULL, y, ...) {


  if(is.factor(y)) {
    lvls <- levels(y)
    tab <- table(y)
    value <- names(tab)[which.max(tab)]
    pct <- tab/sum(tab)
  } else {
    lvls <- NULL
    pct <- NULL
    if(is.null(dim(y))) {
      value <- mean(y, na.rm = TRUE)
    } else {
      value <- colMeans(y, na.rm = TRUE)
    }
  }

  structure(
    list(call = match.call(),
         value = value,
         levels = lvls,
         pct = pct,
         n = length(y[[1]])),
    class = "nullmodel")
}

#' @export
#' @rdname nullmodel
print.nullmodel <- function(x, ...) {
  cat("Null",
      ifelse(is.null(x$levels), "Classification", "Regression"),
      "Model\n")
  x$call

  if (length(x$value) == 1) {
    cat("Predicted Value:",
        ifelse(is.null(x$levels), format(x$value), x$value),
        "\n")
  } else {
    cat("Predicted Value:\n",
        names(x$value), "\n",
        x$value,
        "\n")
  }
}

#' @export
#' @rdname nullmodel
predict.nullmodel <- function (object, new_data = NULL, type  = NULL, ...) {
  if(is.null(type)) {
    type <- if(is.null(object$levels)) "raw" else "class"
  }

  n <- if(is.null(new_data)) object$n else nrow(new_data)
  if(!is.null(object$levels)) {
    if(type == "prob") {
      out <- matrix(rep(object$pct, n), nrow = n, byrow = TRUE)
      colnames(out) <- object$levels
      out <- as.data.frame(out)
    } else {
      out <- factor(rep(object$value, n), levels = object$levels)
    }
  } else {
    if(type %in% c("prob", "class")) stop("Only numeric predicitons are applicable to regression models")
    if(length(object$value) == 1) {
      out <- rep(object$value, n)
    } else {
      out <- as_tibble(matrix(rep(object$value, n),
                              ncol = length(object$value), byrow = TRUE))

      names(out) <- names(object$value)
    }
  }
  out
}

#' General Interface for null models
#'
#' `null_model()` is a way to generate a _specification_ of a model before
#'  fitting and allows the model to be created using R. It doens't have any
#'  main arguments.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @details The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"parsnip"`
#' }
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call.  For this type of
#'  model, the template of the fit calls are:
#'
#' \pkg{parsnip} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::null_model(mode = "classification"), "parsnip")}
#'
#' \pkg{parsnip} regression
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::null_model(mode = "regression"), "parsnip")}
#'
#' @importFrom purrr map_lgl
#' @seealso [varying()], [fit()]
#' @examples
#' null_model(mode = "regression")
#' @export
null_model <-
  function(mode = "classification") {
    null_model_modes <- unique(get_model_env()$null_model$mode)
    # Check for correct mode
    if (!(mode %in% null_model_modes))
      stop("`mode` should be one of: ",
           paste0("'", null_model_modes, "'", collapse = ", "),
           call. = FALSE)

    # Capture the arguments in quosures
    args <- list()

    # Save some empty slots for future parts of the specification
    out <- list(args = args, eng_args = NULL,
                mode = mode, method = NULL, engine = NULL)

    # set classes in the correct order
    class(out) <- make_classes("null_model")
    out
  }
