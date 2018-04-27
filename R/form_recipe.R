# Class for creating an appropriate formula for a model based on the roles
# contained in the recipe. 

#' @importFrom stats formula

formula.model_spec <- function(x, recipe, ...) {
  rec_vars <- summary(recipe)
  y_names <- rec_vars$variable[rec_vars$role == "outcome"]
  if (length(y_names) > 1)
    form_text <- paste0("cbind(", paste0(y_names, collapse = ","), ")~.")
  else
    form_text <- paste0(y_names, "~.")
  
  form <- try(as.formula(form_text), silent = TRUE)
  if(inherits(form, "try-error"))
    stop("Could not parse the model formula: ", form_text, call. = FALSE)
  form
}


formula.surv_reg <- function(x, recipe, ...) {
  rec_vars <- summary(recipe)
  y_names <- rec_vars$variable[rec_vars$role == "outcome"]
  if (length(y_names) > 2 | length(y_names) < 1)
    stop("There should be 1-2 variables in the `outcome` role.", call. = FALSE)
  cens_names <- rec_vars$variable[rec_vars$role == "censoring var"]
  if (length(cens_names) > 1)
    stop("There should be 0-1 variables in the `censoring` role.", call. = FALSE)
  x_names <- rec_vars$variable[rec_vars$role == "predictor"]
  
  # construct basic formula
  form_text <- paste0("Surv(", paste0(y_names, collapse = ", "))
  if (length(cens_names) == 1)
    form_text <- paste0(form_text, ", ", cens_names, ") ~")
  else
    form_text <- paste0(form_text, ") ~")
  
  if (length(x_names) == 0)
    form_text <- paste0(form_text, "1")
  else
    form_text <- paste0(form_text, paste0(x_names, collapse = "+"))
  
  # engine-speciifc options (e.g. spark needing censor var in text)
  if (!is.null(x$engine) && x$engine == "flexsurv") {
    extra_ind <- which(rec_vars$role %in% flexsurv_params)
    if (length(extra_ind) > 0) {
      extra_terms <- paste0(
        rec_vars$role[extra_ind], "(",
        rec_vars$variable[extra_ind], ")"
        )
      form_text <- paste0(form_text, "+", paste0(extra_terms, collapse = "+"))
    }
    if (any(rec_vars$role == "strata"))
      warning(
        "`flexsurv` does not use the `strata` function; instead use ",
        "the parameter roles for differential values (e.g. `sigma`).",
        call. = FALSE
      )
  }
  
  form <- try(as.formula(form_text), silent = TRUE)
  if(inherits(form, "try-error"))
    stop("Could not parse the model formula: ", form_text, call. = FALSE)
  form
}

flexsurv_params <- c("sigma", "shape", "sdlog", "Q", "k", "P", "S1", "s2")
