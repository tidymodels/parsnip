# glmnet call stack using `predict()` when object has
# classes "_<glmnet-class>" and "model_fit":
#
#  predict()
#   predict._<glmnet-class>(penalty = NULL)
#    predict_glmnet(penalty = NULL)             <-- checks and sets penalty
#     predict.model_fit()                       <-- checks for extra vars in ...
#      predict_numeric()
#       predict_numeric._<glmnet-class>()
#        predict_numeric_glmnet()
#         predict_numeric.model_fit()
#          predict.<glmnet-class>()


# glmnet call stack using `multi_predict` when object has
# classes "_<glmnet-class>" and "model_fit":
#
#  multi_predict()
#   multi_predict._<glmnet-class>(penalty = NULL)
#    predict._<glmnet-class>(multi = TRUE)
#     predict_glmnet(multi = TRUE)            <-- checks and sets penalty
#      predict.model_fit()                    <-- checks for extra vars in ...
#       predict_raw()
#        predict_raw._<glmnet-class>()
#         predict_raw_glmnet()
#          predict_raw.model_fit(opts = list(s = penalty))
#           predict.<glmnet-class>()


predict_glmnet <- function(object,
                           new_data,
                           type = NULL,
                           opts = list(),
                           penalty = NULL,
                           multi = FALSE,
                           ...) {
  # See discussion in https://github.com/tidymodels/parsnip/issues/195
  if (is.null(penalty) & !is.null(object$spec$args$penalty)) {
    penalty <- object$spec$args$penalty
  }

  object$spec$args$penalty <- .check_glmnet_penalty_predict(penalty, object, multi)

  object$spec <- eval_args(object$spec)
  predict.model_fit(object, new_data = new_data, type = type, opts = opts, ...)
}

predict_numeric_glmnet <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_numeric.model_fit(object, new_data = new_data, ...)
}

predict_class_glmnet <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_class.model_fit(object, new_data = new_data, ...)
}

predict_classprob_glmnet <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  predict_classprob.model_fit(object, new_data = new_data, ...)
}

predict_raw_glmnet <- function(object, new_data, opts = list(), ...)  {
  object$spec <- eval_args(object$spec)

  opts$s <- object$spec$args$penalty

  predict_raw.model_fit(object, new_data = new_data, opts = opts, ...)
}

# translation of glmnet classes to parsnip models
# elnet ~ linear_reg
# lognet ~ logistic_reg
# multnet ~ multinom_reg
# glmnetfit: that's a catch-all class for glmnet models fitted with a base-R
#  family, thus can be any of linear_reg, logistic_reg, multinom_reg, poisson_reg

#' @export
predict._elnet <- predict_glmnet

#' @export
predict_numeric._elnet <- predict_numeric_glmnet

#' @export
predict_raw._elnet <- predict_raw_glmnet

#' @export
predict._lognet <- predict_glmnet

#' @export
predict_class._lognet <- predict_class_glmnet

#' @export
predict_classprob._lognet <- predict_classprob_glmnet

#' @export
predict_raw._lognet <- predict_raw_glmnet

#' @export
predict._multnet <- predict_glmnet

#' @export
predict_class._multnet <- predict_class_glmnet

#' @export
predict_classprob._multnet <- predict_classprob_glmnet

#' @export
predict_raw._multnet <- predict_raw_glmnet

#' @export
predict._glmnetfit <- predict_glmnet

#' @export
predict_numeric._glmnetfit <- predict_numeric_glmnet

#' @export
predict_class._glmnetfit <- predict_class_glmnet

#' @export
predict_classprob._glmnetfit <- predict_classprob_glmnet

#' @export
predict_raw._glmnetfit <- predict_raw_glmnet

#' Organize glmnet predictions
#'
#' This function is for developer use and organizes predictions from glmnet
#' models.
#'
#' @param x Predictions as returned by the `predict()` method for glmnet models.
#' @param object An object of class `model_fit`.
#'
#' @rdname glmnet_helpers_prediction
#' @keywords internal
#' @export
.organize_glmnet_pred <- function(x, object) {
  unname(x[, 1])
}

organize_glmnet_pre_pred <- function(x, object) {
  x <- x[, rownames(object$fit$beta), drop = FALSE]
  if (is_sparse_matrix(x)) {
    return(x)
  }
  
  as.matrix(x)
}


organize_glmnet_class <- function(x, object) {
  prob_to_class_2(x[, 1], object)
}

organize_glmnet_prob <- function(x, object) {
  res <- tibble(v1 = 1 - x[, 1], v2 = x[, 1])
  colnames(res) <- object$lvl
  res
}

organize_multnet_class <- function(x, object) {
  if (vec_size(x) > 1) {
    x <- x[,1]
  } else {
    x <- as.character(x)
  }
  x
}

organize_multnet_prob <- function(x, object) {
  if (vec_size(x) > 1) {
    x <- as_tibble(x[,,1])
  } else {
    x <- tibble::as_tibble_row(x[,,1])
  }
  x
}

# -------------------------------------------------------------------------

multi_predict_glmnet <- function(object,
                                 new_data,
                                 type = NULL,
                                 penalty = NULL,
                                 ...) {
  type <- check_pred_type(object, type)
  check_spec_pred_type(object, type)
  if (type == "prob") {
    check_spec_levels(object)
  }

  dots <- list(...)

  if (object$spec$mode == "classification") {
    if (is_quosure(penalty)) {
      penalty <- eval_tidy(penalty)
    }
  }

  object$spec <- eval_args(object$spec)

  if (is.null(penalty)) {
    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    if (!is.null(object$spec$args$penalty)) {
      penalty <- object$spec$args$penalty
    } else {
      penalty <- object$fit$lambda
    }
  }

  model_type <- class(object$spec)[1]

  if (object$spec$mode == "classification") {
    if (type == "prob" |
        model_type == "logistic_reg") {
      dots$type <- "response"
    } else {
      dots$type <- type
    }
  }

  pred <- predict(object, new_data = new_data, type = "raw",
                  opts = dots, penalty = penalty, multi = TRUE)


  res <- switch(
    model_type,
    "linear_reg" = format_glmnet_multi_linear_reg(pred, penalty = penalty),
    "logistic_reg" = format_glmnet_multi_logistic_reg(pred,
                                                      penalty = penalty,
                                                      type = type,
                                                      lvl = object$lvl),
    "multinom_reg" = format_glmnet_multi_multinom_reg(pred,
                                                      penalty = penalty,
                                                      type = type,
                                                      lvl = object$lvl,
                                                      n_obs = nrow(new_data))
  )

  res
}

#' @export
#' @rdname multi_predict
#' @param penalty A numeric vector of penalty values.
multi_predict._elnet <- multi_predict_glmnet

#' @export
#' @rdname multi_predict
multi_predict._lognet <- multi_predict_glmnet

#' @export
#' @rdname multi_predict
multi_predict._multnet <- multi_predict_glmnet

#' @export
#' @rdname multi_predict
multi_predict._glmnetfit <- multi_predict_glmnet

format_glmnet_multi_linear_reg <- function(pred, penalty) {
  penalty_key <- tibble(s = colnames(pred), penalty = penalty)

  pred <- as_tibble(pred)
  pred$.row <- seq_len(nrow(pred))
  pred <- tidyr::pivot_longer(pred, -.row, names_to = "s", values_to = ".pred")

  pred <- dplyr::full_join(penalty_key, pred, by = "s", multiple = "all")

  pred <- pred |>
    dplyr::select(-s) |>
    dplyr::arrange(penalty) |>
    tidyr::nest(.by = .row, .key = ".pred") |>
    dplyr::select(-.row)

  pred
}

format_glmnet_multi_logistic_reg <- function(pred, penalty, type, lvl) {
  type <- rlang::arg_match(type, c("class", "prob"))

  penalty_key <- tibble(s = colnames(pred), penalty = penalty)

  pred <- as_tibble(pred)
  pred$.row <- seq_len(nrow(pred))
  pred <- tidyr::pivot_longer(pred, -.row, names_to = "s", values_to = ".pred")

  if (type == "class") {
    pred <- pred |>
      dplyr::mutate(.pred_class = dplyr::if_else(.pred >= 0.5, lvl[2], lvl[1]),
                    .pred_class = factor(.pred_class, levels = lvl),
                    .keep = "unused")
  } else {
    pred <- pred |>
      dplyr::mutate(.pred_class_2 = 1 - .pred) |>
      rlang::set_names(c(".row", "s", paste0(".pred_", rev(lvl)))) |>
      dplyr::select(c(".row", "s", paste0(".pred_", lvl)))
  }

  pred <- dplyr::full_join(penalty_key, pred, by = "s", multiple = "all")

  pred <- pred |>
    dplyr::select(-s) |>
    dplyr::arrange(penalty) |>
    tidyr::nest(.by = .row, .key = ".pred") |>
    dplyr::select(-.row)

  pred
}

format_glmnet_multi_multinom_reg <- function(pred, penalty, type, lvl, n_obs) {
  type <- rlang::arg_match(type, c("class", "prob"))

  pred <- switch(
    type,
    prob = format_glmnet_multinom_prob(pred, penalty, lvl, n_obs),
    class = format_glmnet_multinom_class(pred, penalty, lvl, n_obs)
  )

  pred <- pred |>
    dplyr::arrange(.row, penalty) |>
    tidyr::nest(.by = .row, .key = ".pred") |>
    dplyr::select(-.row)

  pred
}

format_glmnet_multinom_prob <- function(pred, penalty, lvl, n_obs) {
  # pred is an array with
  # dim 1 = observations
  # dim 2 = levels of the response
  # dim 3 = penalty values
  apply(pred, 3, as_tibble) |>
    purrr::list_rbind() |>
    rlang::set_names(paste0(".pred_", lvl)) |>
    dplyr::mutate(
      .row = rep(seq_len(n_obs), times = length(penalty)),
      penalty = rep(penalty, each = n_obs)
    ) |>
    dplyr::relocate(penalty)
}

format_glmnet_multinom_class <- function(pred, penalty, lvl, n_obs) {
  # pred is a matrix n_obs x n_penalty
  # unless n_obs == 1, then it's a vector of length n_penalty
  tibble(
    .row = rep(seq_len(n_obs), times = length(penalty)),
    penalty = rep(penalty, each = n_obs),
    .pred_class = factor(as.vector(pred), levels = lvl)
  )
}

# -------------------------------------------------------------------------

#' Helper functions for checking the penalty of glmnet models
#'
#' @description
#' These functions are for developer use.
#'
#' `.check_glmnet_penalty_fit()` checks that the model specification for fitting a
#' glmnet model contains a single value.
#'
#' `.check_glmnet_penalty_predict()` checks that the penalty value used for prediction is valid.
#' If called by `predict()`, it needs to be a single value. Multiple values are
#' allowed for `multi_predict()`.
#'
#' @param x An object of class `model_spec`.
#' @rdname glmnet_helpers
#' @keywords internal
#' @export
.check_glmnet_penalty_fit <- function(x, call = rlang::caller_env()) {
  pen <- rlang::eval_tidy(x$args$penalty)

  if (length(pen) != 1) {
    cli::cli_abort(
      c(
        "x" = "For the glmnet engine, {.arg penalty} must be a single number
        (or a value of {.fn tune}).",
        "!" = "There are {length(pen)} value{?s} for {.arg penalty}.",
        "i" = "To try multiple values for total regularization, use the
        {.pkg tune} package.",
        "i" = "To predict multiple penalties, use {.fn multi_predict}."
      ),
      call = call
    )
  }
}

#' @param penalty A penalty value to check.
#' @param object An object of class `model_fit`.
#' @param multi A logical indicating if multiple values are allowed.
#'
#' @rdname glmnet_helpers
#' @keywords internal
#' @export
.check_glmnet_penalty_predict <- function(penalty = NULL, object, multi = FALSE,
                                          call = rlang::caller_env()) {
  if (is.null(penalty)) {
    penalty <- object$fit$lambda
  }

  # when using `predict()`, allow for a single lambda
  if (!multi) {
    if (length(penalty) != 1) {
      cli::cli_abort(
        c(
          "{.arg penalty} should be a single numeric value.",
          "i" = "{.fn multi_predict} can be used to get multiple predictions per row of data."
        ),
        call = call
      )
    }
  }

  if (length(object$fit$lambda) == 1 && penalty != object$fit$lambda) {
    cli::cli_abort(
      c(
        "The glmnet model was fit with a single penalty value of
      {.arg object$fit$lambda}. Predicting with a value of {.arg penalty}
      will give incorrect results from `glmnet()`."
      ),
      call = call
    )
  }

  penalty
}

set_glmnet_penalty_path <- function(x) {
  if (any(names(x$eng_args) == "path_values")) {
    # Since we decouple the parsnip `penalty` argument from being the same
    # as the glmnet `lambda` value, `path_values` allows users to set the
    # path differently from the default that glmnet uses. See
    # https://github.com/tidymodels/parsnip/issues/431
    x$method$fit$args$lambda <- x$eng_args$path_values
    x$eng_args$path_values <- NULL
    x$method$fit$args$path_values <- NULL
  } else {
    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    x$method$fit$args$lambda <- NULL
  }
  x
}

