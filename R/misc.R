#' Prepend a new class
#'
#' This adds an extra class to a base class of "model_spec".
#'
#' @param prefix A character string for a class.
#' @return A character vector.
#' @keywords internal
#' @export
make_classes <- function(prefix) {
  c(prefix, "model_spec")
}

#' Check to ensure that ellipses are empty
#' @param ... Extra arguments.
#' @return If an error is not thrown (from non-empty ellipses), a NULL list.
#' @keywords internal
#' @export
check_empty_ellipse <- function (...)  {
  terms <- quos(...)
  if (!is_empty(terms))
    stop("Please pass other arguments to the model function via `set_engine`", call. = FALSE)
  terms
}

all_modes <- c("classification", "regression")


deparserizer <- function(x, limit = options()$width - 10) {
  x <- deparse(x, width.cutoff = limit)
  x <- gsub("^    ", "", x)
  x <- paste0(x, collapse = "")
  if (nchar(x) > limit)
    x <- paste0(substring(x, first = 1, last = limit - 7), "<snip>")
  x
}

print_arg_list <- function(x, ...) {
  atomic <- vapply(x, is.atomic, logical(1))
  x2 <- x
  x2[!atomic] <-  lapply(x2[!atomic], deparserizer, ...)
  res <- paste0("  ", names(x2), " = ", x2, collaspe = "\n")
  cat(res, sep = "")
}

#' Print helper for model objects
#'
#' A common format function that prints information about the model object (e.g.
#' arguments, calls, packages, etc).
#'
#' @param x A model object.
#' @param ... Not currently used.
#' @keywords internal
#' @export
model_printer <- function(x, ...) {
  non_null_args <- x$args[!vapply(x$args, null_value, lgl(1))]
  if (length(non_null_args) > 0) {
    cat("Main Arguments:\n")
    non_null_args <- map(non_null_args, convert_arg)
    cat(print_arg_list(non_null_args), "\n", sep = "")
  }
  if (length(x$eng_args) > 0) {
    cat("Engine-Specific Arguments:\n")
    x$eng_args <- map(x$eng_args, convert_arg)
    cat(print_arg_list(x$eng_args), "\n", sep = "")
  }
  if (!is.null(x$engine)) {
    cat("Computational engine:", x$engine, "\n\n")
    if (!is.null(x$method$fit_call)) {
      cat("Fit function:\n")
      print(x$method$fit_call)
      if (length(x$method$libs) > 0) {
        if (length(x$method$libs) > 1)
          cat("\nRequired packages:\n")
        else
          cat("\nRequired package: ")
        cat(paste0(x$method$libs, collapse = ", "), "\n")
      }
    }
  }
}

is_missing_arg <- function(x)
  identical(x, quote(missing_arg()))


#' Print the model call
#'
#' @param x A "model_spec" object.
#' @return A character string.
#' @keywords internal
#' @export
show_call <- function(object) {
  object$method$fit$args <-
    map(object$method$fit$args, convert_arg)
  if (
    is.null(object$method$fit$func["pkg"]) ||
    is.na(object$method$fit$func["pkg"])
  ) {
    res <- call2(object$method$fit$func["fun"], !!!object$method$fit$args)
  } else {
    res <-
      call2(object$method$fit$func["fun"],
            !!!object$method$fit$args,
            .ns = object$method$fit$func["pkg"])
  }
  res
}

convert_arg <- function(x) {
  if (is_quosure(x))
    quo_get_expr(x)
  else
    x
}

make_call <- function(fun, ns, args, ...) {

  #args <- map(args, convert_arg)

  # remove any null or placeholders (`missing_args`) that remain
  discard <-
    vapply(args, function(x)
      is_missing_arg(x) | is.null(x), logical(1))
  args <- args[!discard]

  if (!is.null(ns) & !is.na(ns)) {
    out <- call2(fun, !!!args, .ns = ns)
  } else
    out <- call2(fun, !!!args)
  out
}

resolve_args <- function(args, ...) {
  for (i in seq(along = args)) {
    if (!is_missing_arg(args[[i]]))
      args[[i]] <- eval_tidy(args[[i]], ...)
  }
  args
}

levels_from_formula <- function(f, dat) {
  if (inherits(dat, "tbl_spark"))
    res <- NULL
  else
    res <- levels(eval_tidy(f[[2]], dat))
  res
}

is_spark <- function(x)
  isTRUE(unname(x$method$fit$func["pkg"] == "sparklyr"))


show_fit <- function(mod, eng) {
  mod <- translate(x = mod, engine = eng)
  fit_call <- show_call(mod)
  call_text <-  deparse(fit_call)
  call_text <- paste0(call_text, collapse = "\n")
  paste0(
    "\\preformatted{\n",
    call_text,
    "\n}\n\n"
  )
}

# Check non-translated core arguments
# Each model has its own definition of this
check_args <- function(object) {
  UseMethod("check_args")
}

check_args.default <- function(object) {
  invisible(object)
}

# ------------------------------------------------------------------------------

# copied form recipes

names0 <- function (num, prefix = "x") {
  if (num < 1)
    stop("`num` should be > 0", call. = FALSE)
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}


# ------------------------------------------------------------------------------

update_dot_check <- function(...) {
  dots <- enquos(...)
  if (length(dots) > 0)
    stop("Extra arguments will be ignored: ",
         paste0("`", names(dots), "`", collapse = ", "),
         call. = FALSE)
  invisible(NULL)
}

# ------------------------------------------------------------------------------

new_model_spec <- function(cls, args, eng_args, mode, method, engine) {
  spec_modes <- get(paste0(cls, "_modes"))
  if (!(mode %in% spec_modes))
    stop("`mode` should be one of: ",
         paste0("'", spec_modes, "'", collapse = ", "),
         call. = FALSE)

  out <- list(args = args, eng_args = eng_args,
              mode = mode, method = method, engine = engine)
  class(out) <- make_classes(cls)
  out
}

# ------------------------------------------------------------------------------

check_outcome <- function(y, spec) {
  if (spec$mode == "unknown") {
    return(invisible(NULL))
  } else if (spec$mode == "regression") {
    if (!is.numeric(y))
      stop("The model outcome should be numeric for regression models.",
           call. = FALSE)
  } else if (spec$mode == "classification") {
    if (!is.factor(y)) {
      stop("The model outcome should be a factor for regression models.",
           call. = FALSE)
    }
  }
  invisible(NULL)
}

