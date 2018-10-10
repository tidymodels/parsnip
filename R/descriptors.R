#' @name descriptors
#' @aliases descriptors n_obs n_cols n_preds n_facts n_levs
#' @title Data Set Characteristics Available when Fitting Models
#' @description When using the `fit` functions there are some
#'  variables that will be available for use in arguments. For
#'  example, if the user would like to choose an argument value
#'  based on the current number of rows in a data set, the `n_obs`
#'  variable can be used. See Details below.
#' @details
#' Existing variables:
#'   \itemize{
#'   \item `n_obs`: the current number of rows in the data set.
#'   \item `n_cols`: the number of columns in the data set that are
#'     associated with the predictors prior to dummy variable creation.
#'   \item `n_preds`: the number of predictors after dummy variables
#'     are created (if any).
#'   \item `n_facts`: the number of factor predictors in the dat set.
#'   \item `n_levs`: If the outcome is a factor, this is a table
#'     with the counts for each level (and `NA` otherwise)
#'   }
#'
#' For example, if you use the model formula `Sepal.Width ~ .` with the `iris`
#'  data, the values would be
#' \preformatted{
#'  n_cols  =   4     (the 4 columns in `iris`)
#'  n_preds =   5     (3 numeric columns + 2 from Species dummy variables)
#'  n_obs   = 150
#'  n_levs  =  NA     (no factor outcome)
#'  n_facts =   1     (the Species predictor)
#' }
#'
#' If the formula `Species ~ .` where used:
#' \preformatted{
#'  n_cols  =   4     (the 4 numeric columns in `iris`)
#'  n_preds =   4     (same)
#'  n_obs   = 150
#'  n_levs  =  c(setosa = 50, versicolor = 50, virginica = 50)
#'  n_facts =   0
#' }
#'
#' To use these in a model fit, either `expression` or `rlang::expr` can be
#' used to delay the evaluation of the argument value until the time when the
#' model is run via `fit` (and the variables listed above are available).
#' For example:
#'
#' \preformatted{
#' library(rlang)
#'
#' data("lending_club")
#'
#' rand_forest(mode = "classification", mtry = expr(n_cols - 2))
#' }
#'
#' When no instance of `expr` is found in any of the argument
#'  values, the descriptor calculation code will not be executed.
#'
NULL

make_descr_env <- function(formula, data) {
  if (inherits(data, "tbl_spark")) {
    res <- make_descr_env_spark(formula, data)
  } else {
    res <- make_descr_env_form(formula, data)
  }
  res
}

make_descr_env_form <- function(formula, data) {
  tmp_dat <- convert_form_to_xy_fit(formula, data, indicators = FALSE)

  if(is.factor(tmp_dat$y)) {
    n_levs <- table(tmp_dat$y, dnn = NULL)
  } else n_levs <- NA

  n_cols <- ncol(tmp_dat$x)
  n_preds <- ncol(convert_form_to_xy_fit(formula, data, indicators = TRUE)$x)
  n_obs <- nrow(data)
  n_facts <- sum(vapply(tmp_dat$x, is.factor, logical(1)))

  env_poke(descr_env, ".data",  data)
  env_poke(descr_env, ".x",     tmp_dat$x)
  env_poke(descr_env, ".y",     tmp_dat$y)
  env_poke(descr_env, ".cols",  n_cols)
  env_poke(descr_env, ".preds", n_preds)
  env_poke(descr_env, ".obs",   n_obs)
  env_poke(descr_env, ".levs",  n_levs)
  env_poke(descr_env, ".facts", n_facts)

  invisible(NULL)
}

#' @importFrom dplyr collect select group_by summarise pull tally
#' @importFrom purrr map map_lgl map_dbl imap imap_lgl as_vector
#' @importFrom stats terms
#' @importFrom rlang syms sym
#' @importFrom utils head
make_descr_env_spark <- function(formula, data) {

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

  env_poke(descr_env, ".cols",  length(f_term_labels))
  env_poke(descr_env, ".preds", preds)
  env_poke(descr_env, ".obs",   dplyr::tally(data) %>% dplyr::pull())
  env_poke(descr_env, ".levs",  y_vals)
  env_poke(descr_env, ".facts", factor_pred)

  invisible(NULL)
}

make_descr_env_xy <- function(x, y) {
  if(is.factor(y)) {
    n_levs <- table(y, dnn = NULL)
  } else n_levs <- NA

  n_cols  <- ncol(x)
  n_preds <- ncol(x)
  n_obs   <- nrow(x)
  n_facts <- if(is.data.frame(x))
    sum(vapply(x, is.factor, logical(1)))
  else
    sum(apply(x, 2, is.factor)) # would this always be zero?

  if(!is.data.frame(x))
    dat <- as.data.frame(x)
  else
    dat <- x
  dat$.y <- y

  env_poke(descr_env, ".data",  dat)
  env_poke(descr_env, ".x",     x)
  env_poke(descr_env, ".y",     y)
  env_poke(descr_env, ".cols",  n_cols)
  env_poke(descr_env, ".preds", n_preds)
  env_poke(descr_env, ".obs",   n_obs)
  env_poke(descr_env, ".levs",  n_levs)
  env_poke(descr_env, ".facts", n_facts)

  invisible(NULL)
}

has_exprs <- function(x) {
  if(is.null(x) | is_varying(x) | is_missing_arg(x))
    return(FALSE)
  is_symbolic(x)
}

make_descr <- function(object) {
  if (length(object$args) > 0)
    expr_main <- map_lgl(object$args, has_exprs)
  else
    expr_main <- FALSE
  if (length(object$others) > 0)
    expr_others <- map_lgl(object$others, has_exprs)
  else
    expr_others <- FALSE
  any(expr_main) | any(expr_others)
}

#' @export
dat <- function() {
  env_get(descr_env, ".data")
}

#' @export
x <- function() {
  env_get(descr_env, ".x")
}

#' @export
y <- function() {
  env_get(descr_env, ".y")
}

#' @export
cols <- function() {
  env_get(descr_env, ".cols")
}

#' @export
preds <- function() {
  env_get(descr_env, ".preds")
}

#' @export
obs <- function() {
  env_get(descr_env, ".obs")
}

#' @export
levs <- function() {
  env_get(descr_env, ".levs")
}

#' @export
facts <- function() {
  env_get(descr_env, ".facts")
}
