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
  varying_args <- map_lgl(x$args, find_varying)
  varying_eng_args <- map_lgl(x$eng_args, find_varying)
  res <- c(varying_args, varying_eng_args)
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

  if (!is.null(id) && !is.character(id)) {
    stop ("`id` should be a single character string.", call. = FALSE)
  }

  id <- id[1]

  if (is.null(id)) {
    id <- deparse(cl$x)
  }

  # Grab the step class before the subset, as that removes the class
  step_type <- class(x)[1]

  # Remove NULL argument steps. These are reserved
  # for deprecated args or those set at prep() time.
  x <- x[!map_lgl(x, is.null)]

  res <- map_lgl(x, find_varying)

  # ensure the user didn't specify a non-varying argument as varying()
  validate_only_allowed_step_args(res, step_type)

  # remove the non-varying arguments as they are not important
  res <- res[!(names(x) %in% non_varying_step_arguments)]

  tibble(
    name = names(res),
    varying = unname(res),
    id = id,
    type = caller_method(cl)
  )
}

validate_only_allowed_step_args <- function(x, step_type) {

  check_allowed_arg <- function(x, nm) {

    # not varying
    if (rlang::is_false(x)) {
      return(invisible(x))
    }

    # not a non-varying step arg name
    bad_nm <- nm %in% non_varying_step_arguments
    if (!bad_nm) {
      return(invisible(x))
    }

    rlang::abort(glue::glue(
      "The following argument for a recipe step of type ",
      "'{step_type}' is not allowed to vary: '{nm}'."
    ))
  }

  purrr::iwalk(x, check_allowed_arg)
  invisible(x)
}

non_varying_step_arguments <- c(
  "terms", "role", "trained", "skip",
  "na.rm", "impute_with", "seed",
  "prefix", "naming", "denom", "outcome", "id"
)

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

  # STEP 1 - Early exits

  # Early exit for empty elements (like list())
  if (length(x) == 0L) {
    return(FALSE)
  }

  # turn quosures into expressions before continuing
  if (is_quosure(x)) {
    x <- quo_get_expr(x)
  }

  if (is_varying(x)) {
    return(TRUE)
  }

  if (is.atomic(x) | is.name(x)) {
    return(FALSE)
  }

  # STEP 2 - Recursion

  varying_elems <- vector("logical", length = length(x))

  for (i in seq_along(x)) {
    varying_elems[i] <- find_varying(x[[i]])
  }

  any_varying_elems <- any(varying_elems)

  return(any_varying_elems)
}

caller_method <- function(cl) {
  x <- cl[[1]]
  x <- deparse(x)
  x <- gsub("varying_args.", "", x, fixed = TRUE)
  x
}

