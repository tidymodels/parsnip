#' @name descriptors
#' @aliases descriptors .obs .cols .preds .facts .lvls .x .y .dat
#' @title Data Set Characteristics Available when Fitting Models
#' @description When using the `fit()` functions there are some
#'  variables that will be available for use in arguments. For
#'  example, if the user would like to choose an argument value
#'  based on the current number of rows in a data set, the `.obs()`
#'  function can be used. See Details below.
#' @details
#' Existing functions:
#'   \itemize{
#'   \item `.obs()`: The current number of rows in the data set.
#'   \item `.preds()`: The number of columns in the data set that is
#'     associated with the predictors prior to dummy variable creation.
#'   \item `.cols()`: The number of predictor columns available after dummy
#'     variables are created (if any).
#'   \item `.facts()`: The number of factor predictors in the data set.
#'   \item `.lvls()`: If the outcome is a factor, this is a table
#'     with the counts for each level (and `NA` otherwise).
#'   \item `.x()`: The predictors returned in the format given. Either a
#'   data frame or a matrix.
#'   \item `.y()`: The known outcomes returned in the format given. Either
#'   a vector, matrix, or data frame.
#'   \item `.dat()`: A data frame containing all of the predictors and the
#'   outcomes. If `fit_xy()` was used, the outcomes are attached as the
#'   column, `..y`.
#'   }
#'
#' For example, if you use the model formula `circumference ~ .` with the
#' built-in `Orange` data, the values would be
#' \preformatted{
#'  .preds() =   2          (the 2 remaining columns in `Orange`)
#'  .cols()  =   5          (1 numeric column + 4 from Tree dummy variables)
#'  .obs()   = 35
#'  .lvls()  =  NA          (no factor outcome)
#'  .facts() =   1          (the Tree predictor)
#'  .y()     = <vector>     (circumference as a vector)
#'  .x()     = <data.frame> (The other 2 columns as a data frame)
#'  .dat()   = <data.frame> (The full data set)
#' }
#'
#' If the formula `Tree ~ .` were used:
#' \preformatted{
#'  .preds() =   2          (the 2 numeric columns in `Orange`)
#'  .cols()  =   2          (same)
#'  .obs()   = 35
#'  .lvls()  =  c("1" = 7, "2" = 7, "3" = 7, "4" = 7, "5" = 7)
#'  .facts() =   0
#'  .y()     = <vector>     (Tree as a vector)
#'  .x()     = <data.frame> (The other 2 columns as a data frame)
#'  .dat()   = <data.frame> (The full data set)
#' }
#'
#' To use these in a model fit, pass them to a model specification.
#' The evaluation is delayed until the time when the
#' model is run via `fit()` (and the variables listed above are available).
#' For example:
#'
#' \preformatted{
#'
#' library(modeldata)
#' data("lending_club")
#'
#' rand_forest(mode = "classification", mtry = .cols() - 2)
#' }
#'
#' When no descriptors are found, the computation of the descriptor values
#' is not executed.
#'
NULL

#' @export
#' @rdname descriptors
.cols <- function() descr_env$.cols()

#' @export
#' @rdname descriptors
.preds <- function() descr_env$.preds()

#' @export
#' @rdname descriptors
.obs <- function() descr_env$.obs()

#' @export
#' @rdname descriptors
.lvls <- function() descr_env$.lvls()

#' @export
#' @rdname descriptors
.facts <- function() descr_env$.facts()

#' @export
#' @rdname descriptors
.x <- function() descr_env$.x()

#' @export
#' @rdname descriptors
.y <- function() descr_env$.y()

#' @export
#' @rdname descriptors
.dat <- function() descr_env$.dat()

# Descriptor retrievers --------------------------------------------------------

get_descr_form <- function(formula, data, call = rlang::caller_env()) {
  if (inherits(data, "tbl_spark")) {
    res <- get_descr_spark(formula, data)
  } else {
    res <- get_descr_df(formula, data, call = call)
  }
  res
}

get_descr_df <- function(formula, data, call = rlang::caller_env()) {

  tmp_dat <-
    .convert_form_to_xy_fit(formula,
                            data,
                            indicators = "none",
                            remove_intercept = TRUE,
                            call = call)

  if(is.factor(tmp_dat$y)) {
    .lvls <- function() {
      table(tmp_dat$y, dnn = NULL)
    }
  } else .lvls <- function() { NA }

  .preds <- function() {
    ncol(tmp_dat$x)
  }

  .cols <- function() {
    ncol(
      .convert_form_to_xy_fit(
        formula,
        data,
        indicators = "traditional",
        remove_intercept = TRUE,
        call = call
      )$x
    )
  }

  .obs <- function() {
    nrow(data)
  }

  .facts <- function() {
    sum(vapply(tmp_dat$x, is.factor, logical(1)))
  }

  .dat <- function() {
    data
  }

  .x <- function() {
    tmp_dat$x
  }

  .y <- function() {
    tmp_dat$y
  }

  list(
    .cols = .cols,
    .preds = .preds,
    .obs = .obs,
    .lvls = .lvls,
    .facts = .facts,
    .dat = .dat,
    .x = .x,
    .y = .y
  )
}

get_descr_spark <- function(formula, data) {

  all_vars <- all.vars(formula)

  if("." %in% all_vars){
    tmpdata <- dplyr::collect(head(data, 1000))
    f_terms <- stats::terms(formula, data = tmpdata)
    f_cols <- rownames(attr(f_terms, "factors"))
  } else {
    f_terms <- stats::terms(formula)
    f_cols <- rownames(attr(f_terms, "factors"))
    term_data <- dplyr::select(data, !!! rlang::syms(f_cols))
    tmpdata <- dplyr::collect(head(term_data, 1000))
  }

  f_term_labels <- attr(f_terms, "term.labels")
  y_ind <- attr(f_terms, "response")
  y_col <- f_cols[y_ind]

  classes <- purrr::map(tmpdata, class)
  icats <- purrr::map_lgl(classes, ~.x == "character")
  cats <- classes[icats]
  cat_preds <- purrr::imap_lgl(cats, ~.y %in% f_term_labels)
  cats <- cats[cat_preds]
  cat_levels <- imap(
    cats,
    ~{
      p <- dplyr::group_by(data, !! rlang::sym(.y))
      p <- dplyr::summarise(p)
      dplyr::pull(p)
    }
  )
  numeric_pred <- length(f_term_labels) - length(cat_levels)


  if(length(cat_levels) > 0){
    n_dummies <- purrr::map_dbl(cat_levels, ~length(.x) - 1)
    n_dummies <- sum(n_dummies)
    all_preds <- numeric_pred + n_dummies
    factor_pred <- length(cat_levels)
  } else {
    factor_pred <- 0
    all_preds <- numeric_pred
  }

  out_cats <- classes[icats]
  out_cats <- out_cats[names(out_cats) == y_col]

  outs <- purrr::imap(
    out_cats,
    ~{
      p <- dplyr::group_by(data, !! sym(.y))
      p <- dplyr::tally(p)
      dplyr::collect(p)
    }
  )

  if(length(outs) > 0){
    outs <- outs[[1]]
    y_vals <- purrr::as_vector(outs[,2])
    names(y_vals) <- purrr::as_vector(outs[,1])
    y_vals <- y_vals[order(names(y_vals))]
    y_vals <- as.table(y_vals)
  } else y_vals <- NA

  obs <- dplyr::tally(data) |> dplyr::pull()

  .cols  <- function() all_preds
  .preds <- function() length(f_term_labels)
  .obs   <- function() obs
  .lvls  <- function() y_vals
  .facts <- function() factor_pred
  .x       <- function() cli::cli_abort("Descriptor {.fn .x} not defined for Spark.")
  .y       <- function() cli::cli_abort("Descriptor {.fn .y} not defined for Spark.")
  .dat     <- function() cli::cli_abort("Descriptor {.fn .dat} not defined for Spark.")

  # still need .x(), .y(), .dat() ?

  list(
    .cols  = .cols,
    .preds = .preds,
    .obs = .obs,
    .lvls = .lvls,
    .facts = .facts,
    .dat = .dat,
    .x = .x,
    .y = .y
  )
}

get_descr_xy <- function(x, y, call = rlang::caller_env()) {

  .lvls <- if (is.factor(y)) {
    function() table(y, dnn = NULL)
  } else {
    function() NA
  }

  .cols  <- function() {
    ncol(x)
  }

  .preds <- function() {
    ncol(x)
  }

  .obs   <- function() {
    nrow(x)
  }

  .facts <- function() {
    if(is.data.frame(x))
      sum(vapply(x, is.factor, logical(1)))
    else
      sum(apply(x, 2, is.factor)) # would this always be zero?
  }

  .dat <- function() {
    .convert_xy_to_form_fit(x, y, remove_intercept = TRUE, call = call)$data
  }

  .x <- function() {
    x
  }

  .y <- function() {
    y
  }

  list(
    .cols  = .cols,
    .preds = .preds,
    .obs = .obs,
    .lvls = .lvls,
    .facts = .facts,
    .dat = .dat,
    .x = .x,
    .y = .y
  )
}

has_exprs <- function(x) {
  if(is.null(x) | is_varying(x) | is_missing_arg(x))
    return(FALSE)
  is_symbolic(x)
}

# Locate descriptors -----------------------------------------------------------

# take a model spec, see if any require descriptors
requires_descrs <- function(object) {
  any(c(
    map_lgl(object$args, has_any_descrs),
    map_lgl(object$eng_args, has_any_descrs)
  ))
}

# given a quosure arg, does the expression contain a descriptor function?
has_any_descrs <- function(x) {

  .x_expr <- rlang::get_expr(x)
  .x_env  <- rlang::get_env(x, parent.frame())

  # evaluated value
  # required so we don't pass an empty env to findGlobals(), which is an error
  if (identical(.x_env, rlang::empty_env())) {
    return(FALSE)
  }

  # globals::globalsOf() is recursive and finds globals if the user passes
  # in a function that wraps a descriptor fn
  .globals <- globals::globalsOf(
    expr = .x_expr,
    envir = .x_env,
    mustExist = FALSE
  )

  .globals <- names(.globals)

  any(map_lgl(.globals, is_descr))
}

is_descr <- function(x) {

  descrs <- list(
    ".cols",
    ".preds",
    ".obs",
    ".lvls",
    ".facts",
    ".x",
    ".y",
    ".dat"
  )

  any(map_lgl(descrs, identical, y = x))
}

# Helpers for overwriting descriptors temporarily ------------------------------

# descrs = list of functions that actually eval to .cols()
poke_descrs <- function(descrs) {

  descr_names <- names(descr_env)

  old <- purrr::map(descr_names, ~{
    descr_env[[.x]]
  })

  names(old) <- descr_names

  purrr::walk(descr_names, ~{
    descr_env[[.x]] <- descrs[[.x]]
  })

  invisible(old)
}

# frame = evaluation frame of when the on.exit() call is made
# we generally set it to whatever fn calls scoped_descrs()
# which should be inside of fit()
scoped_descrs <- function(descrs, frame = caller_env()) {
  old <- poke_descrs(descrs)

  # Inline everything so the call will succeed in any environment
  expr <- call2(on.exit, call2(poke_descrs, old), add = TRUE)
  rlang::eval_bare(expr, frame)

  invisible(old)
}

# Environment that descriptors are found in.
# Originally set to error. At fit time, these are temporarily overriden
# with their actual implementations
descr_env <- rlang::new_environment(
  data = list(
    .cols  = function() cli::cli_abort("Descriptor context not set"),
    .preds = function() cli::cli_abort("Descriptor context not set"),
    .obs   = function() cli::cli_abort("Descriptor context not set"),
    .lvls  = function() cli::cli_abort("Descriptor context not set"),
    .facts = function() cli::cli_abort("Descriptor context not set"),
    .x     = function() cli::cli_abort("Descriptor context not set"),
    .y     = function() cli::cli_abort("Descriptor context not set"),
    .dat   = function() cli::cli_abort("Descriptor context not set")
  )
)
