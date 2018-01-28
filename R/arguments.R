#' @importFrom rlang eval_tidy is_quosure na_lgl lgl is_missing ll


# `sub_arg_values`` takes an existing expression and substitutes 
# different argument values that are passed to it. `ignore` is 
# an optional list of arguments that will not have their arguments 
# substituted. 

## TODO: test this with something containing ... in definition

sub_arg_values <- function (expr, args, ignore = NULL)  {
  arg_names <- names(args)
  # Remove ignore
  if (!is.null(ignore))
    for (i in ignore) {
      if (i %in% arg_names) {
        warning("`", i, "` was not changed in the expression",
                call. = FALSE)
        args[[i]] <- NULL
      }
    }
  arg_names <- names(args)
  
  # Look for `param` in argument list and quote
  
  expr_names <- expr_names(expr)
  
  dot_ind <- dot_index(expr)
  
  missing_arg_names <- arg_names[!(arg_names %in% expr_names)]

  # If any args not in expression list, see if there are ellipses
  # to put them into
  if (length(missing_arg_names) > 0) {
    if (dot_ind == 0) {
      stop(
        "Argument(s) not valid for `",
        expr[[1]],
        "`: ",
        paste0(missing_arg_names, collapse = ", "),
        call. = FALSE
      )
    }
    # Add args for ellipses at end of expression
    for(i in missing_arg_names) {
      expr[[i]] <- args[[i]]
      args[[i]] <- NULL
    }
  } 
  
  arg_names <- names(args)
  
  # Replace argument values with user-specified values which could be
  # evaluated objects (i.e. constants like `TRUE`, `200`, etc), quosures, or calls. 
  
  # Q: Skip arguments that are NULL? This makes sense but might be good for 
  # resetting arguments but would be bad for non-engine options. Maybe make 
  # an option for `sub_arg_values` 
  
  for (i in arg_names) {
    if (!null_value(args[[i]])) {
      if (should_eval(args[[i]])) {
        expr[[i]] <- eval_tidy(args[[i]])
      } else {
        expr[[i]] <- if (is_quosure(args[[i]]))
          args[[i]][[-1]]
        else
          args[[i]]
      }
    }
  }

  expr
}

## assumes that ellipses have a value such as 
## `... = missing_arg()``
dot_index <- function(x) {
  res <- which(names(x) == "...")
  if(length(res) == 0)
    res <- 0
  res
}

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


should_eval <- function(x) {
  length(func_calls(x)) == 0
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


expr_names <- function(x) {
  nms <- names(x)
  no_names <- nms == ""
  if (any(no_names)) {
    for(i in which(no_names))
      nms[i] <- deparse(x[[i]]) 
  }
  nms
}

prune_expr <- function(x, whitelist, modified) {
  nms <- names(x)
  nms <- nms[nms != ""]
  nms <- nms[!(nms %in%  whitelist)]
  for (i in nms) {
    if (is.null(x[[i]]) | is_null(x[[i]]) | !(i %in% modified) | is_missing(x[[i]]))
      x[[i]] <- NULL
  }
  x
}

varying_param_check <- function(x) {
  varies <- vapply(x$method$fit, does_it_vary, lgl(1))
  if(any(varies))
    stop("One or more arguments are not finalized (", 
         paste0("`", names(varies)[varies], "`", collapse = ", "), ")")
  invisible(NULL)
}

#' A Placeholder Function for Argument Values
#' 
#' [varying()] is used when a parameter will be specified at a later date. 
#' @export
varying <- function()
  stop("This is a placeholder and should not be evaluated")

deharmonize <- function(args, key, engine) {
  nms <- names(args)
  for(i in seq_along(args)) {
    names(args)[i] <- key[ nms[i] , engine ]
  }
  args
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

# finalizing the model consists of:
#
# 1. obtaining the base expression for the model
# 2. converting standardized arguments to their engine-specific names
# 3. substituting in the user-specified argument values
# 4. removing any of the original default arguments
#
# This should be done only when the model is to be fit.

#' Resolve a Model Specification for a Computational Engine
#' 
#' `finalize` will translate a model specification into a code
#'  object that is specific to a particular engine (e.g. R package).
#'  It translates generic parameters to their counterparts.
#' 
#' @param x A model specification.
#' @param ... Not currently used. 
#' @export
#' 
finalize <- function (x, ...)
  UseMethod("finalize")



