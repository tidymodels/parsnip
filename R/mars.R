# Prototype parsnip code for multivariate adaptive regression splines (MARS)
#'
#' General Interface for MARS
#'
#' `mars` is a way to generate a _specification_ of a model before
#'  fitting and allows the model to be created using R. The main
#'  arguments for the
#'  model are:
#' \itemize{
#'   \item \code{num_terms}: The number of features that will be retained in the
#'    final model.
#'   \item \code{prod_degree}: The highest possible degree of interaction between
#'    features. A value of 1 indicates and additive model while a value of 2
#'    allows, but does not guarantee, two-way interactions between features.
#'   \item \code{prune_method}: The type of pruning. Possible values are listed
#'    in `?earth`.
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
#'  underlying models (e.g., `earth::earth`, etc.). If the outcome is a factor
#'  and `mode = "classification"`, `others` can include the `glm` argument to
#'  `earth::earth`. If this argument is not passed, it will be added prior to
#'  the fitting occurs.
#' @param num_terms The number of features that will be retained in the
#'    final model, including the intercept.
#' @param prod_degree The highest possible interaction degree.
#' @param prune_method The pruning method.
#' @param ... Used for method consistency. Any arguments passed to
#'  the ellipses will result in an error. Use `others` instead.
#' @details Main parameter arguments (and those in `others`) can avoid
#'  evaluation until the underlying function is executed by wrapping the
#'  argument in [rlang::expr()].
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"earth"`
#' }
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call. These can be changed by using the `others`
#'  argument to pass in the preferred values. For this type of
#'  model, the template of the fit calls are:
#'
#' \pkg{earth} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::mars(mode = "classification"), "earth")}
#'
#' \pkg{earth} regression
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::mars(mode = "regression"), "earth")}
#'
#' Note that, when the model is fit, the \pkg{earth} package only has its
#'  namespace loaded. However, if `multi_predict` is used, the package is
#'  attached.
#'
#' @importFrom purrr map_lgl
#' @seealso [varying()], [fit()]
#' @examples
#' mars(mode = "regression", num_terms = 5)
#' @export

mars <-
  function(mode = "unknown",
           num_terms = NULL, prod_degree = NULL, prune_method = NULL,
           others = list(),
           ...) {
    check_empty_ellipse(...)

    if (!(mode %in% mars_modes))
      stop("`mode` should be one of: ",
           paste0("'", mars_modes, "'", collapse = ", "),
           call. = FALSE)

    if (is.numeric(prod_degree) && prod_degree < 0)
      stop("`prod_degree` should be >= 1", call. = FALSE)
    if (is.numeric(num_terms) && num_terms < 0)
      stop("`num_terms` should be >= 1", call. = FALSE)
    if (!does_it_vary(prune_method) &&
        !is.null(prune_method) &&
        !is.character(prune_method))
      stop("`prune_method` should be a single string value", call. = FALSE)

    args <- list(num_terms = num_terms,
                 prod_degree = prod_degree,
                 prune_method = prune_method)

    no_value <- !vapply(others, is.null, logical(1))
    others <- others[no_value]

    out <- list(args = args, others = others,
                mode = mode, method = NULL, engine = NULL)
    class(out) <- make_classes("mars")
    out
  }

#' @export
print.mars <- function(x, ...) {
  cat("MARS Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @export
#' @inheritParams mars
#' @param object A MARS model specification.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @return An updated model specification.
#' @examples
#' model <- mars(num_terms = 10, prune_method = "none")
#' model
#' update(model, num_terms = 1)
#' update(model, num_terms = 1, fresh = TRUE)
#' @method update mars
#' @rdname mars
#' @export
update.mars <-
  function(object,
           num_terms = NULL, prod_degree = NULL, prune_method = NULL,
           others = list(),
           fresh = FALSE,
           ...) {
    check_empty_ellipse(...)

    args <- list(num_terms = num_terms,
                 prod_degree = prod_degree,
                 prune_method = prune_method)

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
translate.mars <- function(x, engine, ...) {

  # If classification is being done, the `glm` options should be used. Check to
  # see if it is there and, if not, add the default value.
  if (x$mode == "classification") {
    if (!("glm" %in% names(x$others))) {
      x$others$glm <- quote(list(family = stats::binomial))
    }
  }

  x <- translate.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

#' @importFrom purrr map_dfr
earth_submodel_pred <- function(object, new_data, terms = 2:3, ...) {
  map_dfr(terms, earth_reg_updater, object = object, newdata = new_data, ...)
}

#' @importFrom tibble as_tibble tibble
#' @importFrom stats update
earth_reg_updater <- function(num, object, new_data, ...) {
  object <- update(object, nprune = num)
  pred <- predict(object, new_data, ...)
  if (ncol(pred) == 1) {
    res <- tibble::tibble(.pred = pred[, 1], nprune = num)
  } else {
    res <- tibble::as_tibble(res)
    names(res) <- paste0(".pred_", names(res))
    res$nprune <- num
  }
  res
}


# earth helpers ----------------------------------------------------------------

#' @importFrom purrr map_df
#' @importFrom dplyr arrange
#' @export
multi_predict._earth <-
  function(object, new_data, type = NULL, num_terms = NULL, ...) {
    if (is.null(num_terms))
      num_terms <- object$fit$selected.terms[-1]

    num_terms <- sort(num_terms)

    msg <-
      paste("Please use `keepxy = TRUE` as an option to enable submodel",
            "predictions with `earth`.")
    if (any(names(object$spec$others) == "keepxy")) {
      if(!object$spec$others$keepxy)
        stop (msg, call. = FALSE)
    } else
      stop (msg, call. = FALSE)

    if (!exists("earth"))
      suppressPackageStartupMessages(attachNamespace("earth"))

    if (is.null(type)) {
      if (object$spec$mode == "classification")
        type <- "class"
      else
        type <- "numeric"
    }

    res <-
      map_df(num_terms, earth_by_terms, object = object,
             new_data = new_data, type = type, ...)
    res <- arrange(res, .row, num_terms)
    res <- split(res[, -1], res$.row)
    names(res) <- NULL
    tibble(.pred = res)
  }

earth_by_terms <- function(num_terms, object, new_data, type, ...) {
  object$fit <- update(object$fit, nprune = num_terms)
  pred <- predict(object, new_data = new_data, type = type)
  nms <- names(pred)
  pred[["num_terms"]] <- num_terms
  pred[[".row"]] <- 1:nrow(new_data)
  pred[, c(".row", "num_terms", nms)]
}
