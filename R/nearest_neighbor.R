#' K-nearest neighbors
#'
#' @description
#'
#' `nearest_neighbor()` defines a model that uses the `K` most similar data
#' points from the training set to predict new samples. This function can
#' fit classification and regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("nearest_neighbor")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param mode A single character string for the prediction outcome mode.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting.
#' @param neighbors A single integer for the number of neighbors
#' to consider (often called `k`). For \pkg{kknn}, a value of 5
#' is used if `neighbors` is not specified.
#' @param weight_func A *single* character for the type of kernel function used
#' to weight distances between samples. Valid choices are: `"rectangular"`,
#' `"triangular"`, `"epanechnikov"`, `"biweight"`, `"triweight"`,
#' `"cos"`, `"inv"`, `"gaussian"`, `"rank"`, or `"optimal"`.
#' @param dist_power A single number for the parameter used in
#' calculating Minkowski distance.
#'
#' @templateVar modeltype nearest_neighbor
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("nearest_neighbor")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("nearest_neighbor")
#'
#' nearest_neighbor(neighbors = 11)
#'
#' @export
nearest_neighbor <- function(mode = "unknown",
                             engine = "kknn",
                             neighbors = NULL,
                             weight_func = NULL,
                             dist_power = NULL) {
  args <- list(
    neighbors   = enquo(neighbors),
    weight_func = enquo(weight_func),
    dist_power  = enquo(dist_power)
  )

  new_model_spec(
    "nearest_neighbor",
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

#' @method update nearest_neighbor
#' @export
#' @rdname parsnip_update
update.nearest_neighbor <- function(object,
                                    parameters = NULL,
                                    neighbors = NULL,
                                    weight_func = NULL,
                                    dist_power = NULL,
                                    fresh = FALSE, ...) {

  args <- list(
    neighbors   = enquo(neighbors),
    weight_func = enquo(weight_func),
    dist_power  = enquo(dist_power)
  )

  update_spec(
    object = object,
    parameters = parameters,
    args_enquo_list = args,
    fresh = fresh,
    cls = "nearest_neighbor",
    ...
  )
}

# ------------------------------------------------------------------------------

#' @export
check_args.nearest_neighbor <- function(object, call = rlang::caller_env()) {

  args <- lapply(object$args, rlang::eval_tidy)

  check_number_whole(args$neighbors, min = 0, allow_null = TRUE, call = call, arg = "neighbors")
  check_string(args$weight_func, allow_null = TRUE, call = call, arg = "weight_func")
  
  invisible(object)
}

# ------------------------------------------------------------------------------

#' @export
translate.nearest_neighbor <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'kknn'` for translation.")
    engine <- "kknn"
  }
  x <- translate.default(x, engine, ...)

  arg_vals <- x$method$fit$args

  if (engine == "kknn") {
    load_libs(x, quiet = TRUE, attach = TRUE)

    if (!any(names(arg_vals) == "ks") || is_missing_arg(arg_vals$ks)) {
      arg_vals$ks <- 5
    }

    ## -----------------------------------------------------------------------------
    # Protect some arguments based on data dimensions

    if (any(names(arg_vals) == "ks")) {
      arg_vals$ks <-
        rlang::call2("min_rows", rlang::eval_tidy(arg_vals$ks), expr(data), 5)
    }
  }

  x$method$fit$args <- arg_vals

  x
}


# ------------------------------------------------------------------------------

#' @rdname multi_predict
#' @param neighbors An integer vector for the number of nearest neighbors.
#' @export
multi_predict._train.kknn <-
  function(object, new_data, type = NULL, neighbors = NULL, ...) {
    if (is.null(neighbors))
      neighbors <- rlang::eval_tidy(object$fit$call$ks)
    neighbors <- sort(neighbors)

    if (is.null(type)) {
      if (object$spec$mode == "classification")
        type <- "class"
      else
        type <- "numeric"
    }

    res <-
      purrr::map(neighbors, knn_by_k, object = object,
                 new_data = new_data, type = type, ...) |>
      purrr::list_rbind()
    res <- dplyr::arrange(res, .row, neighbors)
    res <- split(res[, -1], res$.row)
    names(res) <- NULL
    dplyr::tibble(.pred = res)
  }

knn_by_k <- function(k, object, new_data, type, ...) {
  object$fit$best.parameters$k <- k

  predict(object, new_data = new_data, type = type, ...) |>
    dplyr::mutate(neighbors = k, .row = dplyr::row_number()) |>
    dplyr::select(.row, neighbors, dplyr::starts_with(".pred"))
}
