library(purrr)
library(rlang)

param <- function(name) NULL

# `adjust_expression`` takes an existing expression and substitutes 
# different argument values that are passed to it. `removals` is 
# an optional list of arguments that will not have their arguments 
# substituted. 


## TODO: test this with something containing ... in definition

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
  
  # NOTE: arguments with no defaults do not have names (see `glm`)
  
  expr_names <- names(expr)
  expr_names <- expr_names[expr_names != ""]
  
  dot_ind <- dot_index(expr)
  
  missing_args <- arg_names[!(arg_names %in% expr_names)]
  
  # If any args not in expression list, see if there are ellipses
  # to put them into
  if (length(missing_args) > 0) {
    if (dot_ind == 0) {
      stop("Argument(s) ",
           paste0(missing_args, collapse = ","),
           " are not in the call",
           call. = FALSE)
    } else {
      expr[[dot_ind]] <- NULL
    }
  }
  
  arg_names <- names(args)
  for (i in arg_names) {
    if (!null_value(args[[i]])) {
      if (should_eval(args[[i]])) {
        expr[[i]] <- eval_tidy(args[[i]])
      } else {
        expr[[i]] <- args[[i]][[-1]]
      }
    }
  }
  
  # remove dots if they are in call
  dot_ind <- dot_index(expr)
  if(dot_ind != 0)
    expr[[dot_ind]] <- NULL

  expr
}


dot_index <- function(x) {
  # There must be a better way
  is_dots <- rep(na_lgl, length(x))
  for(i in seq_along(x)) 
    is_dots[i] <- is_dots(x[[i]])
  dot_ind <- if (any(is_dots))
    which(is_dots)
  else
    0
  dot_ind
}

is_dots <- function(x) {
  if(!inherits(x, "name")) {
    res <- FALSE
  } else {
    res <- isTRUE(all.equal(x, quote(...)))
  }
  res
}

does_it_vary <- function(x) 
  isTRUE(all.equal(x[[-1]], quote(varying())))

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
