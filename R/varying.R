#' A placeholder function for argument values
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' [varying()] is used when a parameter will be specified at a later date.
#' @export
#' @keywords internal
varying <- function() {
  lifecycle::deprecate_warn("0.1.8", "varying()", "hardhat::tune()")
  quote(varying())
}

#' @export
generics::varying_args

#' Determine varying arguments
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `varying_args()` takes a model specification or a recipe and returns a tibble
#' of information on all possible varying arguments and whether or not they
#' are actually varying.
#'
#' The `id` column is determined differently depending on whether a `model_spec`
#' or a `recipe` is used. For a `model_spec`, the first class is used. For
#' a `recipe`, the unique step `id` is used.
#'
#' @param object A `model_spec` or a `recipe`.
#' @param full A single logical. Should all possible varying parameters be
#' returned? If `FALSE`, then only the parameters that
#' are actually varying are returned.
#'
#' @param ... Not currently used.
#'
#' @return A tibble with columns for the parameter name (`name`), whether it
#' contains _any_ varying value (`varying`), the `id` for the object (`id`),
#' and the class that was used to call the method (`type`).
#'
#' @examplesIf !parsnip:::is_cran_check()
#'
#' # List all possible varying args for the random forest spec
#' rand_forest() |> varying_args()
#'
#' # mtry is now recognized as varying
#' rand_forest(mtry = varying()) |> varying_args()
#'
#' # Even engine specific arguments can vary
#' rand_forest() |>
#'   set_engine("ranger", sample.fraction = varying()) |>
#'   varying_args()
#'
#' # List only the arguments that actually vary
#' rand_forest() |>
#'   set_engine("ranger", sample.fraction = varying()) |>
#'   varying_args(full = FALSE)
#'
#' rand_forest() |>
#'   set_engine(
#'     "randomForest",
#'     strata = Class,
#'     sampsize = varying()
#'   ) |>
#'   varying_args()
#'
#' @rdname varying_args
#' @keywords internal
#' @export
varying_args.model_spec <- function(object, full = TRUE, ...) {
  lifecycle::deprecate_warn("0.1.8", "varying_args()", "tune_args()")

  # use the model_spec top level class as the id
  id <- class(object)[1]

  if (length(object$args) == 0L & length(object$eng_args) == 0L) {
    return(varying_tbl())
  }

  # Locate varying args in spec args and engine specific args
  varying_args <- map_lgl(object$args, find_varying)
  varying_eng_args <- map_lgl(object$eng_args, find_varying)

  res <- c(varying_args, varying_eng_args)

  varying_tbl(
    name = names(res),
    varying = unname(res),
    id = id,
    type = "model_spec",
    full = full
  )
}

# Need to figure out a way to meld the results of varying_args with
#  parameter objects from `dials` or from novel parameters in the user's
#  workspace. Maybe register the parameters in dials and have a way of
#  adding/modifying them? A list vector could be added to these tibbles with
#  the actual parameter objects (and the ranges may need to be set).

# Maybe use this data as substrate to make a new object type (param_set?) that
#  would have its own methods for grids and random sampling.

#' @export
#' @rdname varying_args
varying_args.recipe <- function(object, full = TRUE, ...) {
  lifecycle::deprecate_warn("0.1.8", "varying_args()", "tune_args()")

  steps <- object$steps

  if (length(steps) == 0L) {
    return(varying_tbl())
  }

  map(object$steps, varying_args, full = full) |> purrr::list_rbind()
}

#' @export
#' @rdname varying_args
varying_args.step <- function(object, full = TRUE, ...) {
  lifecycle::deprecate_warn("0.1.8", "varying_args()", "tune_args()")

  # Unique step id
  id <- object$id

  # Grab the step class before the subset, as that removes the class
  step_type <- class(object)[1]

  # Remove NULL argument steps. These are reserved
  # for deprecated args or those set at prep() time.
  object <- object[!map_lgl(object, is.null)]

  res <- map_lgl(object, find_varying)

  # ensure the user didn't specify a non-varying argument as varying()
  validate_only_allowed_step_args(res, step_type)

  # remove the non-varying arguments as they are not important
  res <- res[!(names(object) %in% non_varying_step_arguments)]

  varying_tbl(
    name = names(res),
    varying = unname(res),
    id = id,
    type = "step",
    full = full
  )
}

# useful for standardization and for creating a 0 row varying tbl
# (i.e. for when there are no steps in a recipe)
varying_tbl <- function(name = character(),
                        varying = logical(),
                        id = character(),
                        type = character(),
                        full = FALSE) {

  vry_tbl <- tibble(
    name = name,
    varying = varying,
    id = id,
    type = type
  )

  if (!full) {
    vry_tbl <- vry_tbl[vry_tbl$varying,]
  }

  vry_tbl
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

    cli::cli_abort(
      "The argument {nm} for a recipe step of type
       {.val step_type} is not allowed to vary."
    )
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

#' @export
#' @keywords internal
#' @rdname add_on_exports
is_varying <- function(x) {
  if (is.null(x)) {
    res <- FALSE
  } else {
    if (is_quosure(x)) {
      res <- isTRUE(all.equal(x[[-1]], quote(varying())))
    } else {
      res <- isTRUE(all.equal(x, quote(varying())))
    }
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
