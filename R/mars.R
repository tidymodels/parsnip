#' Multivariate adaptive regression splines (MARS)
#'
#' @description
#'
#' `mars()` defines a generalized linear model that uses artificial features for
#' some predictors. These features resemble hinge functions and the result is
#' a model that is a segmented regression in small dimensions.
#'
#' There are different ways to fit this model. Information about the available
#' _engines_ that can be used for fitting at:
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::find_engine_files("mars")}
#'
#' More information on how `parsnip` is used for model is at
#' \url{https://www.tidymodels.org}.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param num_terms The number of features that will be retained in the
#'    final model, including the intercept.
#' @param prod_degree The highest possible interaction degree.
#' @param prune_method The pruning method.
#' @details
#' This function only defines what _type_ of model is being fit. Once an engine
#'  is specified, the _method_ to fit the model is also defined.
#'
#' The model is not trained or fit until the [fit.model_spec()] function is used
#' with the data.
#'
#' @references \url{https://www.tidymodels.org},
#' [_Tidy Models with R_](https://tmwr.org)
#' @seealso [fit.model_spec()], [set_engine()], [update()],
#' \code{\link[=details_mars_earth]{earth engine details}}
#' @examples
#' show_engines("mars")
#'
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

#' @method update mars
#' @rdname parsnip_update
#' @export
update.mars <-
  function(object,
           parameters = NULL,
           num_terms = NULL, prod_degree = NULL, prune_method = NULL,
           fresh = FALSE, ...) {

    eng_args <- update_engine_parameters(object$eng_args, ...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }

    args <- list(
      num_terms    = enquo(num_terms),
      prod_degree  = enquo(prod_degree),
      prune_method = enquo(prune_method)
    )

    args <- update_main_parameters(args, parameters)

    if (fresh) {
      object$args <- args
      object$eng_args <- eng_args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
      if (length(eng_args) > 0)
        object$eng_args[names(eng_args)] <- eng_args
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
      x$eng_args$glm <- rlang::quo(list(family = stats::binomial))
    }
  }

  x <- translate.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

check_args.mars <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$prod_degree) && args$prod_degree < 0)
    rlang::abort("`prod_degree` should be >= 1.")

  if (is.numeric(args$num_terms) && args$num_terms < 0)
    rlang::abort("`num_terms` should be >= 1.")

  if (!is_varying(args$prune_method) &&
      !is.null(args$prune_method) &&
      !is.character(args$prune_method))
    rlang::abort("`prune_method` should be a single string value.")

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
    names(res) <- paste0(".pred_", names(res))
    res <- tibble::as_tibble(res)
    res$nprune <- num
  }
  res
}


# earth helpers ----------------------------------------------------------------

#' @importFrom purrr map_df
#' @importFrom dplyr arrange
#' @rdname multi_predict
#' @param num_terms An integer vector for the number of MARS terms to retain.
#' @export
multi_predict._earth <-
  function(object, new_data, type = NULL, num_terms = NULL, ...) {
    if (any(names(enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

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
       if (!isTRUE(object$fit$call$keepxy))
         rlang::abort(msg)
    } else {
      rlang::abort(msg)
    }

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
