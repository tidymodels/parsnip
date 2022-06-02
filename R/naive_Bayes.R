#' Naive Bayes models
#'
#' @description
#'
#' `naive_Bayes()` defines a model that uses Bayes' theorem to compute the
#' probability of each class, given the predictor values. This function can fit
#' classification models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("naive_Bayes")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @inheritParams discrim_linear
#' @param smoothness An non-negative number representing the the relative
#'  smoothness of the class boundary. Smaller examples result in model flexible
#'  boundaries and larger values generate class boundaries that are less
#'  adaptable
#' @param Laplace A non-negative value for the Laplace correction to smoothing
#' low-frequency counts.
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("naive_Bayes")}
#' @export
naive_Bayes <-
  function(mode = "classification", smoothness = NULL, Laplace = NULL, engine = "klaR") {
    args <-
      list(
        smoothness = rlang::enexpr(smoothness),
        Laplace = rlang::enexpr(Laplace)
      )

    new_model_spec(
      "naive_Bayes",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.naive_Bayes <- function(x, ...) {
  cat("Naive Bayes Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (is_printable_spec(x)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update naive_Bayes
#' @rdname parsnip_update
#' @inheritParams naive_Bayes
#' @export
update.naive_Bayes <-
  function(object,
           smoothness = NULL, Laplace = NULL,
           fresh = FALSE, ...) {

    args <-
      list(
        smoothness = rlang::enexpr(smoothness),
        Laplace    = rlang::enexpr(Laplace)
      )

    update_spec(
      object = object,
      parameters = NULL,
      args_enquo_list = args,
      fresh = fresh,
      cls = "naive_Bayes",
      ...
    )
  }

# ------------------------------------------------------------------------------

set_new_model("naive_Bayes")
set_model_mode("naive_Bayes", "classification")
