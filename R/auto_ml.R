#' Automatic Machine Learning
#'
#' @description
#'
#' `auto_ml()` defines an automated searching and tuning process where
#' many models of different families are trained and ranked given their
#' performance on the training data.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("auto_ml")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param mode A single character string for the prediction outcome mode.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting.
#'
#' @templateVar modeltype auto_ml
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("auto_ml")}
#' @export
auto_ml <- function(mode = "unknown", engine = "h2o") {
  args <- list()
  out <- list(args = args, eng_args = NULL,
              mode = mode, method = NULL, engine = engine)
  class(out) <- make_classes("auto_ml")
  out
}

# ------------------------------------------------------------------------------
set_new_model("auto_ml")
set_model_mode("auto_ml", "regression")
set_model_mode("auto_ml", "classification")
