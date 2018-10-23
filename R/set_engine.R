#' Declare a computational engine and specific arguments
#' 
#' `set_engine` is used to specify which package or system will be used
#'  to fit the model, along with any arguments specific to that software. 
#'  
#' @param object A model specification. 
#' @param engine A character string for the software that should
#'  be used to fit the model. This is highly dependent on the type
#'  of model (e.g. linear regression, random forest, etc.).
#' @param ... Any optional arguments associated with the chosen computational
#'  engine. These are captured as quosures and can be `varying()`. 
#' @return An updated model specification. 
#' @examples 
#' # First, set general arguments using the standardized names
#' mod <- 
#'   logistic_reg(mixture = 1/3) %>%     
#'   # now say how you want to fit the model and another other options
#'   set_engine("glmnet", nlambda = 10)
#' translate(mod, engine = "glmnet")
#' @export
set_engine <- function(object, engine, ...) {
  if (!is.character(engine) | length(engine) != 1)
    stop("`engine` should be a single character value.", call. = FALSE)
  
  object$engine <- engine
  object <- parsnip:::check_engine(object)
  
  
  object$others  <- enquos(...)
  object
}