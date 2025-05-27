#' Multivariate adaptive regression splines (MARS)
#'
#' @description
#'
#' `mars()` defines a generalized linear model that uses artificial features for
#' some predictors. These features resemble hinge functions and the result is
#' a model that is a segmented regression in small dimensions. This function can
#' fit classification and regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("mars")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams nearest_neighbor
#' @param num_terms The number of features that will be retained in the
#'    final model, including the intercept.
#' @param prod_degree The highest possible interaction degree.
#' @param prune_method The pruning method.
#'
#' @templateVar modeltype mars
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("mars")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("mars")
#'
#' mars(mode = "regression", num_terms = 5)
#' @export
mars <-
  function(mode = "unknown", engine = "earth",
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
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
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

    args <- list(
      num_terms    = enquo(num_terms),
      prod_degree  = enquo(prod_degree),
      prune_method = enquo(prune_method)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "mars",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
translate.mars <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'earth'` for translation.")
    engine <- "earth"
  }
  if (engine == "earth") {
    load_libs(x, quiet = TRUE, attach = TRUE)
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

#' @export
check_args.mars <- function(object, call = rlang::caller_env()) {

  args <- lapply(object$args, rlang::eval_tidy)

  check_number_whole(args$prod_degree, min = 1, allow_null = TRUE, call = call, arg = "prod_degree")
  check_number_whole(args$num_terms, min = 1, allow_null = TRUE, call = call, arg = "num_terms")
  check_string(args$prune_method, allow_empty = FALSE, allow_null = TRUE, call = call, arg = "prune_method")

  invisible(object)
}

# ------------------------------------------------------------------------------

earth_submodel_pred <- function(object, new_data, terms = 2:3, ...) {
  load_libs(object, quiet = TRUE, attach = TRUE)
  map(terms, earth_reg_updater, object = object, newdata = new_data, ...) |>
    purrr::list_rbind()
}

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

#' @rdname multi_predict
#' @param num_terms An integer vector for the number of MARS terms to retain.
#' @export
multi_predict._earth <-
  function(object, new_data, type = NULL, num_terms = NULL, ...) {
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
      c("x" = "Please use {.code keepxy = TRUE} as an option to enable submodel
                     predictions with earth.")
    if (any(names(object$fit$call) == "keepxy")) {
      if (!isTRUE(object$fit$call$keepxy)) {
        cli::cli_abort(msg)
      }
    } else {
      cli::cli_abort(msg)
    }

    if (is.null(type)) {
      if (object$spec$mode == "classification") {
        type <- "class"
      } else {
        type <- "numeric"
      }
    }

    res <-
      map(num_terms, earth_by_terms, object = object,
          new_data = new_data, type = type, ...) |>
      purrr::list_rbind()
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
  pred[[".row"]] <- seq_len(nrow(new_data))
  pred[, c(".row", "num_terms", nms)]
}
