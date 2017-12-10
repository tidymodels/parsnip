library(purrr)
library(rlang)

param <- function(name) NULL

# `adjust_expression`` takes an existing expression and substitutes 
# different argument values that are passed to it. `removals` is 
# an optional list of arguments that will not have their arguments 
# substituted. 

adjust_expression <- function (expr, args, removals = NULL)  {
  arg_names <- names(args)
  # Remove removals
  if (!is.null(removals))
    for (i in removals) {
      if (i %in% arg_names) {
        warning("`", i, "` was not changed in the expression",
                call. = FALSE)
        args[[i]] <- NULL
      }
    }
  arg_names <- names(args)
  
  # Look for `param` in argument list and quote
  
  expr_names <- names(expr)
  expr_names <- expr_names[expr_names != ""]
  
  is_dots <- map_lgl(expr, function(x) isTRUE(all.equal(x, quote(...))))
  dot_ind <- if (any(is_dots))
    which(is_dots)
  else
    0
  
  missing_args <- arg_names[!(arg_names %in% expr_names)]
  
  # If any args not in expression list, see if there are ellipses
  # to put them into
  if (length(missing_args) > 0) {
    if (dot_ind == 0) {
      stop("Arguments ",
           paste0(missing_args, collapse = ","),
           "are not in the call",
           call. = FALSE)
    } else {
      expr[[dot_ind]] <- NULL
    }
  }
  
  arg_names <- names(args)
  for (i in arg_names)
    expr[[i]] <- args[[i]]
  
  # remove dots here too?
  expr
}

