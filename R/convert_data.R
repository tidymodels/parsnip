# ------------------------------------------------------------------------------

#' Helper functions to convert between formula and matrix interface
#'
#' @description
#' Functions to take a formula interface and get the resulting
#' objects (y, x, weights, etc) back or the other way around. The functions are
#' intended for developer use. For the most part, this emulates the internals
#' of `lm()` (and also see the notes at
#' https://developer.r-project.org/model-fitting-functions.html).
#'
#' `.convert_form_to_xy_fit()` and `.convert_xy_to_form_fit()` are for when the
#' data are created for modeling.
#' `.convert_form_to_xy_fit()` saves both the data objects as well as the objects
#' needed when new data are predicted (e.g. `terms`, etc.).
#'
#' `.convert_form_to_xy_new()` and `.convert_xy_to_form_new()` are used when new
#' samples are being predicted and only require the predictors to be available.
#'
#' @param data A data frame containing all relevant variables (e.g. outcome(s),
#'   predictors, case weights, etc).
#' @param ... Additional arguments passed to [stats::model.frame()].
#' @param na.action A function which indicates what should happen when the data
#'   contain NAs.
#' @param indicators A string describing whether and how to create
#'   indicator/dummy variables from factor predictors. Possible options are
#'   `"none"`, `"traditional"`, and `"one_hot"`.
#' @param composition A string describing whether the resulting `x` and `y`
#'   should be returned as a `"matrix"` or a `"data.frame"`.
#' @param remove_intercept A logical indicating whether to remove the intercept
#'   column after `model.matrix()` is finished.
#' @inheritParams fit.model_spec
#' @rdname convert_helpers
#' @keywords internal
#' @export
#'
.convert_form_to_xy_fit <- function(formula,
                                    data,
                                    ...,
                                    na.action = na.omit,
                                    indicators = "traditional",
                                    composition = "data.frame",
                                    remove_intercept = TRUE) {
  if (!(composition %in% c("data.frame", "matrix"))) {
    rlang::abort("`composition` should be either 'data.frame' or 'matrix'.")
  }

  ## Assemble model.frame call from call arguments
  mf_call <- quote(model.frame(formula, data))
  mf_call$na.action <- match.call()$na.action # TODO this should work better
  dots <- quos(...)
  check_form_dots(dots)
  for (i in seq_along(dots)) {
    mf_call[[names(dots)[i]]] <- get_expr(dots[[i]])
  }

  mod_frame <- eval_tidy(mf_call)
  mod_terms <- attr(mod_frame, "terms")

  # Notes for next line. if the lhs of the formula is
  #  - a single numeric  y <- numeric vector
  #  - otherwise, this is a data frame (including cases with 2+
  #    cbound numeric columns, factors, Surv objects, etc).
  y <- model.response(mod_frame, type = "any")

  # if y is a numeric vector, model.response() added names
  if (is.atomic(y)) {
    names(y) <- NULL
  }

  w <- as.vector(model.weights(mod_frame))
  if (!is.null(w) && !is.numeric(w)) {
    rlang::abort("`weights` must be a numeric vector")
  }

  # TODO: Do we actually use the offset when fitting?
  # Extract any inline offsets specified in the formula from the model frame
  offset <- model.offset(mod_frame)

  if (indicators != "none") {
    if (indicators == "one_hot") {
      local_one_hot_contrasts()
    }

    x <- model.matrix(mod_terms, mod_frame)
  } else {
    # this still ignores -vars in formula
    x <- model.frame(mod_terms, data)
    y_cols <- attr(mod_terms, "response")
    if (length(y_cols) > 0) {
      x <- x[, -y_cols, drop = FALSE]
    }
  }

  if (remove_intercept) {
    x <- x[, colnames(x) != "(Intercept)", drop = FALSE]
  }
  options <-
    list(
      indicators = indicators,
      composition = composition,
      remove_intercept = remove_intercept
    )

  if (composition == "data.frame") {
    if (is.matrix(y)) {
      y <- as.data.frame(y)
    }
    res <-
      list(
        x = as.data.frame(x),
        y = y,
        weights = w,
        offset = offset,
        terms = mod_terms,
        xlevels = .getXlevels(mod_terms, mod_frame),
        options = options
      )
  } else {
    # Since a matrix is requested, try to convert y but check
    # to see if it is possible
    if (will_make_matrix(y)) {
      y <- as.matrix(y)
    }
    res <-
      list(
        x = x,
        y = y,
        weights = w,
        offset = offset,
        terms = mod_terms,
        xlevels = .getXlevels(mod_terms, mod_frame),
        options = options
      )
  }
  res
}


#' @param object An object of class `model_fit`.
#' @inheritParams predict.model_fit
#' @rdname convert_helpers
#' @keywords internal
#' @export
.convert_form_to_xy_new <- function(object,
                                    new_data,
                                    na.action = na.pass,
                                    composition = "data.frame") {
  if (!(composition %in% c("data.frame", "matrix"))) {
    rlang::abort("`composition` should be either 'data.frame' or 'matrix'.")
  }

  mod_terms <- object$terms
  mod_terms <- delete.response(mod_terms)

  new_data <-
    model.frame(
      mod_terms,
      new_data,
      na.action = na.action,
      xlev = object$xlevels
    )

  cl <- attr(mod_terms, "dataClasses")
  if (!is.null(cl)) {
    .checkMFClasses(cl, new_data)
  }

  # TODO: Do we actually use the returned offsets anywhere for prediction?
  # Extract offset from model frame. Multiple offsets will be added together.
  # Offsets might have been supplied through the formula.
  offset <- model.offset(new_data)

  if (object$options$indicators != "none") {
    if (object$options$indicators == "one_hot") {
      local_one_hot_contrasts()
    }

    new_data <- model.matrix(mod_terms, new_data)
  }

  if (object$options$remove_intercept) {
    new_data <- new_data[, colnames(new_data) != "(Intercept)", drop = FALSE]
  }

  if (composition == "data.frame") {
    new_data <- as.data.frame(new_data)
  } else {
    if (will_make_matrix(new_data)) {
      new_data <- as.matrix(new_data)
    }
  }
  list(x = new_data, offset = offset)
}

# ------------------------------------------------------------------------------

# The other direction where we make a formula from the data
# objects

# TODO slots for other roles
#' @param weights A numeric vector containing the weights.
#' @param y_name A string specifying the name of the outcome.
#' @inheritParams fit.model_spec
#' @inheritParams .convert_form_to_xy_fit
#'
#' @rdname convert_helpers
#' @keywords internal
#' @export
#'
.convert_xy_to_form_fit <- function(x,
                                    y,
                                    weights = NULL,
                                    y_name = "..y",
                                    remove_intercept = TRUE) {
  if (is.vector(x)) {
    rlang::abort("`x` cannot be a vector.")
  }

  if (remove_intercept) {
    x <- x[, colnames(x) != "(Intercept)", drop = FALSE]
  }

  rn <- rownames(x)

  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }

  if (is.matrix(y)) {
    y <- as.data.frame(y)
  } else {
    if (is.vector(y) | is.factor(y)) {
      y <- data.frame(y)
      names(y) <- y_name
    }
  }

  x_var <- names(x)
  check_dup_names(x, y)
  form <- make_formula(names(x), names(y))

  x <- bind_cols(x, y)
  if (!is.null(rn) & !inherits(x, "tbl_df")) {
    rownames(x) <- rn
  }

  if (!is.null(weights)) {
    if (!is.numeric(weights)) {
      rlang::abort("`weights` must be a numeric vector")
    }
    if (length(weights) != nrow(x)) {
      rlang::abort(glue::glue("`weights` should have {nrow(x)} elements"))
    }

    form <- patch_formula_environment_with_case_weights(
      formula = form,
      data = x,
      case_weights = weights
    )
  }

  res <- list(
    formula = form,
    data = x,
    weights = weights,
    x_var = x_var
  )
  res
}

#' @rdname convert_helpers
#' @keywords internal
#' @export
.convert_xy_to_form_new <- function(object, new_data) {
  new_data <- new_data[, object$x_var, drop = FALSE]
  if (!is.data.frame(new_data)) {
    new_data <- as.data.frame(new_data)
  }
  new_data
}


# ------------------------------------------------------------------------------

local_one_hot_contrasts <- function(frame = rlang::caller_env()) {
  contrasts <- getOption("contrasts")
  contrasts["unordered"] <- "contr_one_hot"

  rlang::local_options(contrasts = contrasts, .frame = frame)
}

check_form_dots <- function(x) {
  good_args <- c("subset", "weights")
  good_names <- names(x) %in% good_args
  if (any(!good_names)) {
    rlang::abort(
      glue::glue(
        "These argument(s) cannot be used to create the data: ",
        glue::glue_collapse(glue::glue("`{names(x)[!good_names]}`"), sep = ", "),
        ". Possible arguments are: ",
        glue::glue_collapse(glue::glue("`{good_args}`"), sep = ", ")
      )
    )
  }
  invisible(NULL)
}

make_formula <- function(x, y, short = TRUE) {
  if (length(y) > 1) {
    y_part <- paste0(
      "cbind(",
      paste0(y, collapse = ","),
      ")~"
    )
  } else
    y_part <- paste0(y, "~")
  if(short)
    form_text <- paste0(y_part, ".")
  else
    form_text <- paste0(y_part, paste0(x, collapse = "+"))
  as.formula(form_text)
}


will_make_matrix <- function(y) {
  if (is.matrix(y) | is.vector(y) | (is.factor(y) & length(y) == 0))
    return(FALSE)
  cls <- unique(unlist(lapply(y, class)))
  if (length(cls) > 1)
    return(FALSE)
  can_convert <-
    vapply(y, function(x)
      is.atomic(x) & !is.factor(x), logical(1))
  all(can_convert)
}

check_dup_names <- function(x, y) {
  if (is.vector(y))
    return(invisible(NULL))

  common_names <- intersect(colnames(x), colnames(y))
  if (length(common_names) > 0)
    rlang::abort(
      glue::glue(
        "`x` and `y` have at least one name in common: ",
        glue::glue_collapse(glue::glue("'{common_names}'"), sep = ", ")
      )
    )
  invisible(NULL)
}

## -----------------------------------------------------------------------------

#' Fuzzy conversions
#'
#' These are substitutes for `as.matrix()` and `as.data.frame()` that leave
#'  a sparse matrix as-is.
#' @param x A data frame, matrix, or sparse matrix.
#' @return A data frame, matrix, or sparse matrix.
#' @export
maybe_matrix <- function(x) {
  inher(x, c("data.frame", "matrix", "dgCMatrix"), cl = match.call())
  if (is.data.frame(x)) {
    non_num_cols <- vapply(x, function(x) !is.numeric(x), logical(1))
    if (any(non_num_cols)) {
      non_num_cols <- names(non_num_cols)[non_num_cols]
      non_num_cols <- glue::glue_collapse(glue::single_quote(non_num_cols), sep = ", ")
      msg <- glue::glue("Some columns are non-numeric. The data cannot be ",
                        "converted to numeric matrix: {non_num_cols}.")
      rlang::abort(msg)
    }
    x <- as.matrix(x)
  }
  # leave alone if matrix or sparse matrix
  x
}

#' @rdname maybe_matrix
#' @export
maybe_data_frame <- function(x) {
  if (!inherits(x, "dgCMatrix")) {
    x <- as.data.frame(x)
  }
  x
}
