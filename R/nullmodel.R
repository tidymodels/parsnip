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
#' `predict.nullmodel()` returns either a factor or numeric vector
#' depending on the class of \code{y}. All predictions are always the same.
#' @keywords models internal
#' @examplesIf !parsnip:::is_cran_check()
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
    if (type %in% c("prob", "class")) {
      cli::cli_abort("Only numeric predicitons are applicable to regression models.")
    }
    if (length(object$value) == 1) {
      out <- rep(object$value, n)
    } else {
      out <- matrix(rep(object$value, n), ncol = length(object$value), byrow = TRUE)
      colnames(out) <- names(object$value)
      out <- as_tibble(out)
    }
  }
  out
}

#' Null model
#'
#' `null_model()` defines a simple, non-informative model. It doesn't have any
#'  main arguments. This function can fit classification and regression models.
#'
#' @param mode A single character string for the type of model. The only
#' possible values for this model are `"regression"` and `"classification"`.
#' @param engine A single character string specifying what computational engine
#' to use for fitting. Possible engines are listed below. The default for this
#' model is `"parsnip"`.
#'
#' @includeRmd man/rmd/null-model.md details
#'
#' @seealso [fit.model_spec()]
#' @examplesIf !parsnip:::is_cran_check()
#' null_model(mode = "regression")
#' @export
null_model <-
  function(mode = "classification", engine = "parsnip") {
    args <- list()

    new_model_spec(
      "null_model",
      args = args,
      eng_args = NULL,
      mode = mode,
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
}



#' Tidy method for null models
#'
#' Return the results of `nullmodel` as a tibble
#'
#' @param x A `nullmodel` object.
#' @param ... Not used.
#' @return A tibble with column `value`.
#' @export
#' @keywords internal
#' @examplesIf !parsnip:::is_cran_check()
#'
#' nullmodel(mtcars[,-1], mtcars$mpg) |> tidy()

tidy.nullmodel <- function(x, ...) {
  tibble::tibble(value = x$value)
}

