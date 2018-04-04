
does_it_vary <- function(x) {
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

null_value <- function(x) {
  res <- if(is_quosure(x))
    isTRUE(all.equal(x[[-1]], quote(NULL))) else
      isTRUE(all.equal(x, NULL))
  res
}

#recipes:::fun_calls
func_calls <- function (f)  {
  if (is.function(f)) {
    func_calls(body(f))
  }
  else if (is.call(f)) {
    fname <- as.character(f[[1]])
    if (identical(fname, ".Internal"))
      return(fname)
    unique(c(fname, unlist(lapply(f[-1], func_calls), use.names = FALSE)))
  }
}

#' A Placeholder Function for Argument Values
#'
#' [varying()] is used when a parameter will be specified at a later date.
#' @export
varying <- function()
  quote(varying())


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

check_others <- function(args, x) {

  # Make sure that we are not trying to modify an argument that
  # is explicitly protected in the method metadata
  common_args <- intersect(x$protect, names(args))
  if (length(common_args) > 0) {
    args <- args[!(names(args) %in% common_args)]
    common_args <- paste0(common_args, collapse = ", ")
    warning("The following arguments cannot be manually modified ",
            "and were removed: ",
            common_args, call. = FALSE)
  }

  # If there are not ellipses in function, make sure that the
  # args exist in the fit call
  if (length(args) > 0 & !x$has_dots) {
    o_names <- names(args)
    missing_args <- o_names[!(o_names %in% names(x$fit))]
    if (length(missing_args) > 0) {
      args[o_names %in% missing_args] <- NULL
      warning(
        "Some argument(s) do not correspond to this function: ",
        paste0("`", o_names, "`", collapse = ", "),
        call. = FALSE
      )
    }
  }
  args
}



