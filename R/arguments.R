#' @import rlang

null_value <- function(x) {
  res <- if(is_quosure(x))
    isTRUE(all.equal(x[[-1]], quote(NULL))) else
      isTRUE(all.equal(x, NULL))
  res
}

deharmonize <- function(args, key, engine) {
  nms <- names(args)
  orig_names <- key[nms, engine]
  names(args) <- orig_names
  args[!is.na(orig_names)]
}

parse_engine_options <- function(x) {
  res <- ll()
  if (length(x) >= 2) { # in case of NULL

    arg_names <- names(x[[2]])
    arg_names <- arg_names[arg_names != ""]

    if (length(arg_names) > 0) {
      # in case of list()
      res <- ll()
      for (i in arg_names) {
        res[[i]] <- x[[2]][[i]]
      } # over arg_names
    } # length == 0
  }
  res
}

prune_arg_list <- function(x, whitelist = NULL, modified = character(0)) {
  nms <- names(x)
  if (length(whitelist) > 0)
    nms <- nms[!(nms %in% whitelist)]
  for (i in nms) {
    if (
      is.null(x[[i]])    |
      is_null(x[[i]])    |
      !(i %in% modified) |
      is_missing(x[[i]])
    )
      x[[i]] <- NULL
  }
  if(any(names(x) == "..."))
    x["..."] <- NULL
  x
}

check_others <- function(args, obj, core_args) {
  # Make sure that we are not trying to modify an argument that
  # is explicitly protected in the method metadata or arg_key
  protected_args <- unique(c(obj$protect, core_args))
  common_args <- intersect(protected_args, names(args))
  if (length(common_args) > 0) {
    args <- args[!(names(args) %in% common_args)]
    common_args <- paste0(common_args, collapse = ", ")
    warning("The following arguments cannot be manually modified ",
            "and were removed: ",
            common_args, call. = FALSE)
  }
  args
}

#' Change elements of a model specification
#'
#' `set_args` can be used to modify the arguments of a model specification while
#'  `set_mode` is used to change the model's mode.
#'
#' @param object A model specification.
#' @param ... One or more named model arguments.
#' @param mode A character string for the model type (e.g. "classification" or
#'  "regression")
#' @return An updated model object.
#' @details `set_args` will replace existing values of the arguments.
#'
#' @examples
#' rand_forest()
#'
#' rand_forest() %>%
#'   set_args(mtry = 3, importance = TRUE) %>%
#'   set_mode("regression")
#'
#' @export
set_args <- function(object, ...) {
  the_dots <- list(...)
  if (length(the_dots) == 0)
    stop("Please pass at least one named argument.", call. = FALSE)
  main_args <- names(object$args)
  new_args <- names(the_dots)
  for (i in new_args) {
    if (any(main_args == i)) {
      object$args[[i]] <- the_dots[[i]]
    } else {
      object$others[[i]] <- the_dots[[i]]
    }
  }
  object
}

#' @rdname set_args
#' @export
set_mode <- function(object, mode) {
  if (is.null(mode))
    return(object)
  mode <- mode[1]
  if (!(any(all_modes == mode))) {
    stop("`mode` should be one of ",
         paste0("'", all_modes, "'", collapse = ", "),
         call. = FALSE)
  }
  object$mode <- mode
  object
}


