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
#'  set using `set_engine`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @inheritParams boost_tree
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param num_terms The number of features that will be retained in the
#'    final model, including the intercept.
#' @param prod_degree The highest possible interaction degree.
#' @param prune_method The pruning method.
#' @details The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"earth"`
#' }
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call.  For this type of
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
           num_terms = NULL, prod_degree = NULL, prune_method = NULL) {

    args <- list(
      num_terms    = enquo(num_terms),
      prod_degree  = enquo(prod_degree),
      prune_method = enquo(prune_method)
    )

    new_model_spec(
      "mars",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
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
#' @inheritParams update.boost_tree
#' @param object A MARS model specification.
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
           fresh = FALSE, ...) {
    update_dot_check(...)

    args <- list(
      num_terms    = enquo(num_terms),
      prod_degree  = enquo(prod_degree),
      prune_method = enquo(prune_method)
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
      "mars",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

#' @export
translate.mars <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'earth'` for translation.")
    engine <- "earth"
  }
  # If classification is being done, the `glm` options should be used. Check to
  # see if it is there and, if not, add the default value.
  if (x$mode == "classification") {
    if (!("glm" %in% names(x$eng_args))) {
      x$eng_args$glm <- quote(list(family = stats::binomial))
    }
  }

  x <- translate.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

check_args.mars <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$prod_degree) && args$prod_degree < 0)
    stop("`prod_degree` should be >= 1", call. = FALSE)

  if (is.numeric(args$num_terms) && args$num_terms < 0)
    stop("`num_terms` should be >= 1", call. = FALSE)

  if (!is_varying(args$prune_method) &&
      !is.null(args$prune_method) &&
      !is.character(args$prune_method))
    stop("`prune_method` should be a single string value", call. = FALSE)

  invisible(object)
}

# ------------------------------------------------------------------------------

#' @importFrom purrr map_dfr
earth_submodel_pred <- function(object, new_data, terms = 2:3, ...) {
  load_libs(object, quiet = TRUE, attach = TRUE)
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
    if (any(names(enquos(...)) == "newdata"))
      stop("Did you mean to use `new_data` instead of `newdata`?", call. = FALSE)

    load_libs(object, quiet = TRUE, attach = TRUE)

    if (is.null(num_terms))
      num_terms <- object$fit$selected.terms[-1]

    num_terms <- sort(num_terms)

    # update.earth uses the values in the call so evaluate them if
    # they are quosures
    call_names <- names(object$fit$call)
    call_names <- call_names[!(call_names %in% c("", "x", "y"))]
    for (i in call_names) {
      if (is_quosure(object$fit$call[[i]]))
        object$fit$call[[i]] <- eval_tidy(object$fit$call[[i]])
    }

    msg <-
      paste("Please use `keepxy = TRUE` as an option to enable submodel",
            "predictions with `earth`.")
    if (any(names(object$fit$call) == "keepxy")) {
       if(!isTRUE(object$fit$call$keepxy))
        stop (msg, call. = FALSE)
    } else
      stop (msg, call. = FALSE)

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
