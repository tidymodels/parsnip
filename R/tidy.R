#' Turn a parsnip model object into a tidy tibble
#'
#' This method tidies the model in a parsnip model object, if it exists.
#'
#' @inheritParams generics::tidy
#'
#' @return a tibble
#' @export
#'
#' @examples
#' library(broom)
#'
#' logistic_reg() %>%
#'   set_engine("glm") %>%
#'   fit(Class ~ funded_amnt + int_rate, data = lending_club) %>%
#'   # tidying model object and passing arguments to broom:::tidy.glm
#'   tidy(conf.int = TRUE, exponentiate = TRUE)
tidy.model_fit <- function(x, ...) {
  tryCatch(generics::tidy(x$fit, ...),
    error = function(error) {
      cat("Error:", error$message)
    }
  )
}
