#' A placeholder function for argument values
#'
#' [varying()] is used when a parameter will be specified at a later date.
#' @export
varying <- function()
  quote(varying())

#' Determine varying arguments
#'
#' `varying_args` takes a model specification and lists all of the arguments
#'  along with whether they are fully specified or not.
#' @param x An object
#' @param id A string describing the object `x`.
#' @param ... Not currently used.
#' @return A tibble with columns for the parameter name (`name`), whether is
#'  contains _any_ varying value (`varying`), the `id` for the object, and the
#'  class that was used to call the method (`type`).
#' @examples
#' library(dplyr)
#' library(rlang)
#'
#' rand_forest() %>% varying_args(id = "plain")
#'
#' rand_forest(mtry = varying()) %>% varying_args(id = "one arg")
#'
#' rand_forest() %>%
#'   set_engine("ranger", sample.fraction = varying()) %>%
#'   varying_args(id = "only eng_args")
#'
#' rand_forest() %>%
#'   set_engine(
#'     "ranger",
#'     strata = expr(Class),
#'      sampsize = c(varying(), varying())
#'   ) %>%
#'   varying_args(id = "add an expr")
#'
#'  rand_forest() %>%
#'    set_engine("ranger", classwt = c(class1 = 1, class2 = varying())) %>%
#'    varying_args(id = "list of values")
#'
#' @export
varying_args <- function (x, id, ...)
  UseMethod("varying_args")

#' @importFrom purrr map map_lgl
#' @export
#' @export varying_args.model_spec
#' @rdname varying_args
varying_args.model_spec <- function(x, id = NULL, ...) {
  cl <- match.call()

  if (!is.null(id) && !is.character(id))
    stop ("`id` should be a single character string.", call. = FALSE)
  id <- id[1]

  if (is.null(id))
    id <- deparse(cl$x)
  varying_args <- map(x$args, find_varying)
  varying_eng_args <- map(x$eng_args, find_varying)
  res <- c(varying_args, varying_eng_args)
  res <- map_lgl(res, any)
  tibble(
    name = names(res),
    varying = unname(res),
    id = id,
    type = caller_method(cl)
  )
}

# NOTE Look at the `sampsize` and `classwt` examples above. Using varying() in
#  a vector will convert it to list. When the model-specific `translate` is
#  run, we should catch those and convert them back to vectors if the varying
#  parameter has been replaced with a real value.

# Need to figure out a way to meld the results of varying_args with
#  parameter objects from `dials` or from novel parameters in the user's
#  workspace. Maybe register the parameters in dials and have a way of
#  adding/modifying them? A list vector could be added to these tibbles with
#  the actual parameter objects (and the ranges may need to be set).

# Maybe use this data as substrate to make a new object type (param_set?) that
#  would have its own methods for grids and random sampling.

# lots of code duplication below and probably poor planning; just a prototype.
# once the generics package is done, these will go into recipes

#' @importFrom purrr map2_dfr map_chr
#' @export
#' @export varying_args.recipe
#' @rdname varying_args
varying_args.recipe <- function(x, id = NULL, ...) {
  step_type <- map_chr(x$steps, function(x) class(x)[1])
  step_type <- make.names(step_type, unique = TRUE) # change with new tibble version
  res <- map2_dfr(x$steps, step_type, varying_args)
  res
}

#' @importFrom purrr map map_lgl
#' @export
#' @export varying_args.step
#' @rdname varying_args
varying_args.step <- function(x, id = NULL, ...) {
  cl <- match.call()
  if (!is.null(id) && !is.character(id))
    stop ("`id` should be a single character string.", call. = FALSE)
  id <- id[1]

  if (is.null(id))
    id <- deparse(cl$x)

  exclude <-
    c("terms", "role", "trained", "skip", "na.rm", "impute_with", "seed",
      "prefix", "naming", "denom", "outcome", "id")
  x <- x[!(names(x) %in% exclude)]
  x <- x[!map_lgl(x, is.null)]
  res <- map(x, find_varying)

  res <- map_lgl(res, any)
  tibble(
    name = names(res),
    varying = unname(res),
    id = id,
    type = caller_method(cl)
  )
}

# helpers ----------------------------------------------------------------------

is_varying <- function(x) {
  if(is.null(x)) {
    res <- FALSE
  } else {
    res <- if(is_quosure(x))
      isTRUE(all.equal(x[[-1]], quote(varying())))
    else
      isTRUE(all.equal(x, quote(varying())))
  }
  res
}

find_varying <- function(x) {
  if (is_quosure(x))
    x <- quo_get_expr(x)
  if (is_varying(x)) {
    return(TRUE)
  } else if (is.atomic(x) | is.name(x)) {
    FALSE
  } else if (is.call(x) || is.pairlist(x)) {
    for (i in seq_along(x)) {
      if (is_varying(x[[i]]))
        return(TRUE)
    }
    FALSE
  } else if (is.vector(x) | is.list(x)) {
    map_lgl(x, find_varying)
  } else {
    # User supplied incorrect input
    stop("Don't know how to handle type ", typeof(x),
         call. = FALSE)
  }
}

caller_method <- function(cl) {
  x <- cl[[1]]
  x <- deparse(x)
  x <- gsub("varying_args.", "", x, fixed = TRUE)
  x
}

