# ------------------------------------------------------------------------------

# Functions to take a formula interface and get the resulting
# objects (y, x, weights, etc) back. For the most part, this
# emulates the internals of `lm` (and also see the notes at
# https://developer.r-project.org/model-fitting-functions.html).

# `convert_form_to_xy_fit` is for when the data are created for modeling.
# It saves both the data objects as well as the objects needed
# when new data are predicted (e.g. `terms`, etc.).

# `convert_form_to_xy_new` is used when new samples are being predicted
# and only requires the predictors to be available.

#' @importFrom stats .checkMFClasses .getXlevels delete.response
#' @importFrom stats model.offset model.weights na.omit na.pass

convert_form_to_xy_fit <-function(
  formula,
  data,
  ...,
  na.action = na.omit,
  indicators = TRUE,
  composition = "data.frame"
) {
  if (!(composition %in% c("data.frame", "matrix")))
    stop("`composition` should be either 'data.frame' or ",
         "'matrix'.",
         call. = FALSE)

  ## Assemble model.frame call from call arguments
  mf_call <- quote(model.frame(formula, data))
  mf_call$na.action <- match.call()$na.action # TODO this should work better
  dots <- quos(...)
  check_form_dots(dots)
  for(i in seq_along(dots))
    mf_call[[ names(dots)[i] ]] <- get_expr(dots[[i]])

  # setup contrasts
  if (any(names(dots) == "contrasts"))
    contrasts <- eval_tidy(dots[["contrasts"]])
  else
    contrasts <- NULL

  # For new data, save the expression to create offsets (if any)
  if (any(names(dots) == "offset"))
    offset_expr <- get_expr(dots[["offset"]])
  else
    offset_expr <- NULL

  mod_frame <- eval_tidy(mf_call)
  mod_terms <- attr(mod_frame, "terms")

  # Notes for next line. if the lhs of the formula is
  #  - a single numeric  y <- numeric vector
  #  - otherwise, this is a data frame (including cases with 2+
  #    cbound numeric columns, factors, Surv objects, etc).
  y <- model.response(mod_frame, type = "any")

  w <- as.vector(model.weights(mod_frame))
  if (!is.null(w) && !is.numeric(w))
    stop("'weights' must be a numeric vector", call. = FALSE)

  offset <- as.vector(model.offset(mod_frame))
  if (!is.null(offset)) {
    if (length(offset) != nrow(mod_frame))
      stop("The offset data should have ", nrow(mod_frame),
           "elements.", call. = FALSE)
  }

  if (indicators) {
    x <- model.matrix(mod_terms, mod_frame, contrasts)
  } else {
    # this still ignores -vars in formula ¯\_(ツ)_/¯
    x <- model.frame(mod_terms, data)
    y_cols <- attr(mod_terms, "response")
    if (length(y_cols) > 0)
      x <- x[,-y_cols, drop = FALSE]
  }

  ## TODO maybe an option not to do this?
  x <- x[, colnames(x) != "(Intercept)", drop = FALSE]

  options <-
    list(
      indicators = indicators,
      composition = composition,
      contrasts = contrasts
    )

  if (composition == "data.frame") {
    if (is.matrix(y))
      y <- as.data.frame(y)
    res <-
      list(
        x = as.data.frame(x),
        y = y,
        weights = w,
        offset = offset,
        terms = mod_terms,
        xlevels = .getXlevels(mod_terms, mod_frame),
        offset_expr = offset_expr,
        options = options
      )
  } else {
    # Since a matrix is requested, try to convert y but check
    # to see if it is possible
    if (will_make_matrix(y))
      y <- as.matrix(y)
    res <-
      list(
        x = x,
        y = y,
        weights = w,
        offset = offset,
        terms = mod_terms,
        xlevels = .getXlevels(mod_terms, mod_frame),
        offset_expr = offset_expr,
        options = options
      )
  }
  res
}

convert_form_to_xy_new <- function(object, new_data, na.action = na.pass,
                           composition = "data.frame") {
  if (!(composition %in% c("data.frame", "matrix")))
    stop("`composition` should be either 'data.frame' or ",
         "'matrix'.",
         call. = FALSE)

  mod_terms <- object$terms
  mod_terms <- delete.response(mod_terms)

  # Calculate offset(s). These can show up in-line in the formula
  # (in multiple places) and might also be as its own argument. If
  # there is more than one offset, we add them together.

  offset_cols <- attr(mod_terms, "offset")

  # If offset was done at least once in-line
  if (!is.null(offset_cols)) {
    offset <- rep(0, nrow(new_data))
    for (i in offset_cols)
      offset <- offset +
        eval_tidy(attr(mod_terms, "variables")[[i + 1]],
                  new_data) # use na.action here and below?
  } else offset <- NULL

  if (!is.null(object$offset_expr)) {
    if (is.null(offset))
      offset <- rep(0, nrow(new_data))
    offset <- offset + eval_tidy(object$offset_expr, new_data)
  }

  new_data <-
    model.frame(mod_terms,
                new_data,
                na.action = na.action,
                xlev = object$xlevels)

  cl <- attr(mod_terms, "dataClasses")
  if (!is.null(cl))
    .checkMFClasses(cl, new_data)

  if(object$options$indicators) {
    new_data <-
      model.matrix(mod_terms, new_data, contrasts.arg = object$contrasts)
  }

  new_data <- new_data[, colnames(new_data) != "(Intercept)", drop = FALSE]

  if (composition == "data.frame")
    new_data <- as.data.frame(new_data)
  else {
    if (will_make_matrix(new_data))
      new_data <- as.matrix(new_data)
  }
  list(x = new_data, offset = offset)
}

# ------------------------------------------------------------------------------

# The other direction where we make a formula from the data
# objects

#' @importFrom dplyr bind_cols
# TODO slots for other roles
convert_xy_to_form_fit <- function(x, y, weights = NULL, y_name = "..y") {
  if (is.vector(x))
    stop("`x` cannot be a vector", call. = FALSE)

  rn <- rownames(x)

  if (!is.data.frame(x))
    x <- as.data.frame(x)

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
  if(!is.null(rn) & !inherits(x, "tbl_df"))
    rownames(x) <- rn

  if (!is.null(weights)) {
    if (!is.numeric(weights))
      stop("'weights' must be a numeric vector", call. = FALSE)
    if (length(weights) != nrow(x))
      stop("`weights` should have ", nrow(x), " elements", call. = FALSE)
  }

  res <- list(
    formula = form,
    data = x,
    weights = weights,
    x_var = x_var
  )
  res
}

convert_xy_to_form_new <- function(object, new_data) {
  new_data <- new_data[, object$x_var, drop = FALSE]
  if (!is.data.frame(new_data))
    new_data <- as.data.frame(new_data)
  new_data
}


# ------------------------------------------------------------------------------

check_form_dots <- function(x) {
  good_args <- c("subset", "weights", "contrasts", "offset")
  good_names <- names(x) %in% good_args
  if (any(!good_names)) {
    stop(
      "These argument(s) cannot be used to create the data: ",
      paste0("`", names(x)[!good_names], "`", collapse = ", "),
      ". Possible arguments are: ",
      paste0("`", good_args, "`", collapse = ", "),
      call. = FALSE
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
  if (is.matrix(y) | is.vector(y))
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
    stop(
      "`x` and `y` have at least one name in common: ",
      paste0("'", common_names, "'", collapse = ", "),
      call. = FALSE
    )
  invisible(NULL)
}
