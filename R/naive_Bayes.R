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
#' @inheritParams nearest_neighbor
#' @inheritParams discrim_linear
#' @param smoothness An non-negative number representing the the relative
#'  smoothness of the class boundary. Smaller examples result in model flexible
#'  boundaries and larger values generate class boundaries that are less
#'  adaptable
#' @param Laplace A non-negative value for the Laplace correction to smoothing
#' low-frequency counts.
#'
#' @templateVar modeltype naive_Bayes
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
        smoothness = rlang::enquo(smoothness),
        Laplace = rlang::enquo(Laplace)
      )

    new_model_spec(
      "naive_Bayes",
      args = args,
      eng_args = NULL,
      mode = mode,
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
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
        smoothness = rlang::enquo(smoothness),
        Laplace    = rlang::enquo(Laplace)
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
