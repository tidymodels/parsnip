#' General Interface for Logistic Regression Models
#'
#' `logistic_reg` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R, Stan, keras, or via Spark. The main 
#'  arguments for the model are:
#' \itemize{
#'   \item \code{penalty}: The total amount of regularization
#'  in the model. Note that this must be zero for some engines.
#'   \item \code{mixture}: The proportion of L1 regularization in
#'  the model. Note that this will be ignored for some engines.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using `set_engine`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update` can be used
#'  in lieu of recreating the object from scratch.
#' @inheritParams boost_tree
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "classification".
#' @param penalty An non-negative number representing the total
#'  amount of regularization (`glmnet`, `keras`, and `spark` only).
#'  For `keras` models, this corresponds to purely L2 regularization
#'  (aka weight decay) while the other models can be a combination
#'  of L1 and L2 (depending on the value of `mixture`).
#' @param mixture A number between zero and one (inclusive) that
#'  represents the proportion of regularization that is used for the
#'  L2 penalty (i.e. weight decay, or ridge regression) versus L1
#'  (the lasso) (`glmnet` and `spark` only).
#' @details
#' For `logistic_reg`, the mode will always be "classification".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"glm"` or `"glmnet"`
#' \item \pkg{Stan}:  `"stan"`
#' \item \pkg{Spark}: `"spark"`
#' \item \pkg{keras}: `"keras"`
#' }
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call.  For this type of
#'  model, the template of the fit calls are:
#'
#' \pkg{glm}
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::logistic_reg(), "glm")}
#'
#' \pkg{glmnet}
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::logistic_reg(), "glmnet")}
#'
#' \pkg{stan}
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::logistic_reg(), "stan")}
#'
#' \pkg{spark}
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::logistic_reg(), "spark")}
#'
#' \pkg{keras}
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::logistic_reg(), "keras")}
#' 
#' When using `glmnet` models, there is the option to pass
#'  multiple values (or no values) to the `penalty` argument.
#'  This can have an effect on the model object results. When using
#'  the `predict` method in these cases, the return object type
#'  depends on the value of `penalty`. If a single value is
#'  given, the results will be a simple numeric vector. When
#'  multiple values or no values for `penalty` are used in
#'  `logistic_reg`, the `predict` method will return a data frame with
#'  columns `values` and `lambda`.
#'
#' For prediction, the `stan` engine can compute posterior
#'  intervals analogous to confidence and prediction intervals. In
#'  these instances, the units are the original outcome and when
#'  `std_error = TRUE`, the standard deviation of the posterior
#'  distribution (or posterior predictive distribution as
#'  appropriate) is returned. For `glm`, the standard error is in logit units
#'  while the intervals are in probability units.
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
#' @seealso [varying()], [fit()]
#' @examples
#' logistic_reg()
#' # Parameters can be represented by a placeholder:
#' logistic_reg(penalty = varying())
#' @export
#' @importFrom purrr map_lgl
logistic_reg <-
  function(mode = "classification",
           penalty = NULL,
           mixture = NULL) {

    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    new_model_spec(
      "logistic_reg",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.logistic_reg <- function(x, ...) {
  cat("Logistic Regression Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @inheritParams update.boost_tree
#' @param object A logistic regression model specification.
#' @examples
#' model <- logistic_reg(penalty = 10, mixture = 0.1)
#' model
#' update(model, penalty = 1)
#' update(model, penalty = 1, fresh = TRUE)
#' @method update logistic_reg
#' @rdname logistic_reg
#' @export
update.logistic_reg <-
  function(object,
           penalty = NULL, mixture = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)
    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
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
      "logistic_reg",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

check_args.logistic_reg <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$penalty)) && any(args$penalty < 0))
    stop("The amount of regularization should be >= 0", call. = FALSE)
  if (is.numeric(args$mixture) && (args$mixture < 0 | args$mixture > 1))
    stop("The mixture proportion should be within [0,1]", call. = FALSE)
  if (is.numeric(args$mixture) && length(args$mixture) > 1)
    stop("Only one value of `mixture` is allowed.", call. = FALSE)

  invisible(object)
}

# ------------------------------------------------------------------------------

prob_to_class_2 <- function(x, object) {
  x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
  unname(x)
}


organize_glmnet_class <- function(x, object) {
  if (ncol(x) == 1) {
    res <- prob_to_class_2(x[, 1], object)
  } else {
    n <- nrow(x)
    res <- utils::stack(as.data.frame(x))
    res$values <- prob_to_class_2(res$values, object)
    if (!is.null(object$spec$args$penalty))
      res$lambda <- rep(object$spec$args$penalty, each = n) else
        res$lambda <- rep(object$fit$lambda, each = n)
    res <- res[, colnames(res) %in% c("values", "lambda")]
  }
  res
}

organize_glmnet_prob <- function(x, object) {
  if (ncol(x) == 1) {
    res <- tibble(v1 = 1 - x[, 1], v2 = x[, 1])
    colnames(res) <- object$lvl
  } else {
    n <- nrow(x)
    res <- utils::stack(as.data.frame(x))
    res <- tibble(v1 = 1 - res$values, v2 = res$values)
    colnames(res) <- object$lvl
    if (!is.null(object$spec$args$penalty))
      res$lambda <- rep(object$spec$args$penalty, each = n) else
        res$lambda <- rep(object$fit$lambda, each = n)
  }
  res
}

# ------------------------------------------------------------------------------

#' @export
predict._lognet <- function (object, new_data, type = NULL, opts = list(), ...) {
  if (any(names(enquos(...)) == "newdata"))
    stop("Did you mean to use `new_data` instead of `newdata`?", call. = FALSE)
  
  object$spec <- eval_args(object$spec)
  predict.model_fit(object, new_data = new_data, type = type, opts = opts, ...)
}

#' @export
predict_class._lognet <- function (object, new_data, ...) {
  if (any(names(enquos(...)) == "newdata"))
    stop("Did you mean to use `new_data` instead of `newdata`?", call. = FALSE)
  
  object$spec <- eval_args(object$spec)
  predict_class.model_fit(object, new_data = new_data, ...)
}

#' @export
predict_classprob._lognet <- function (object, new_data, ...) {
  if (any(names(enquos(...)) == "newdata"))
    stop("Did you mean to use `new_data` instead of `newdata`?", call. = FALSE)
  
  object$spec <- eval_args(object$spec)
  predict_classprob.model_fit(object, new_data = new_data, ...)
}

#' @export
predict_raw._lognet <- function (object, new_data, opts = list(), ...) {
  if (any(names(enquos(...)) == "newdata"))
    stop("Did you mean to use `new_data` instead of `newdata`?", call. = FALSE)
  
  object$spec <- eval_args(object$spec)
  predict_raw.model_fit(object, new_data = new_data, opts = opts, ...)
}


#' @importFrom dplyr full_join as_tibble arrange
#' @importFrom tidyr gather
#' @export
multi_predict._lognet <-
  function(object, new_data, type = NULL, penalty = NULL, ...) {
    if (any(names(enquos(...)) == "newdata"))
      stop("Did you mean to use `new_data` instead of `newdata`?", call. = FALSE)
    
    dots <- list(...)
    if (is.null(penalty))
      penalty <- object$lambda
    dots$s <- penalty

    if (is.null(type))
      type <- "class"
    if (!(type %in% c("class", "prob", "link"))) {
      stop ("`type` should be either 'class', 'link', or 'prob'.", call. = FALSE)
    }
    if (type == "prob")
      dots$type <- "response"
    else
      dots$type <- type

    object$spec <- eval_args(object$spec)
    pred <- predict(object, new_data = new_data, type = "raw", opts = dots)
    param_key <- tibble(group = colnames(pred), penalty = penalty)
    pred <- as_tibble(pred)
    pred$.row <- 1:nrow(pred)
    pred <- gather(pred, group, .pred, -.row)
    if (dots$type == "class") {
      pred[[".pred"]] <- factor(pred[[".pred"]], levels = object$lvl)
    } else {
      if (dots$type == "response") {
        pred[[".pred2"]] <- 1 - pred[[".pred"]]
        names(pred) <- c(".row", "group", paste0(".pred_", rev(object$lvl)))
        pred <- pred[, c(".row", "group", paste0(".pred_", object$lvl))]
      }
    }
    pred <- full_join(param_key, pred, by = "group")
    pred$group <- NULL
    pred <- arrange(pred, .row, penalty)
    .row <- pred$.row
    pred$.row <- NULL
    pred <- split(pred, .row)
    names(pred) <- NULL
    tibble(.pred = pred)
  }

# ------------------------------------------------------------------------------

#' @importFrom utils globalVariables
utils::globalVariables(c("group", ".pred"))
