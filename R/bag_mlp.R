#' Ensembles of neural networks
#'
#' @description
#'
#' `bag_mlp()` defines an ensemble of single layer, feed-forward neural networks.
#' This function can fit classification and regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("bag_mlp")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams mlp
#'
#' @templateVar modeltype bag_mlp
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("bag_mlp")}
#' @export
bag_mlp <-
  function(mode = "unknown",
           hidden_units = NULL,
           penalty = NULL,
           epochs = NULL,
           engine = "nnet") {
    args <- list(
      hidden_units   = enquo(hidden_units),
      penalty  = enquo(penalty),
      epochs  = enquo(epochs)
    )

    new_model_spec(
      "bag_mlp",
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

#' @method update bag_mlp
#' @rdname parsnip_update
#' @inheritParams mars
#' @export
update.bag_mlp <-
  function(object,
           parameters = NULL,
           hidden_units = NULL, penalty = NULL, epochs = NULL,
           fresh = FALSE, ...) {

    args <- list(
      hidden_units   = enquo(hidden_units),
      penalty  = enquo(penalty),
      epochs  = enquo(epochs)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "bag_mlp",
      ...
    )
  }

# ------------------------------------------------------------------------------

set_new_model("bag_mlp")
set_model_mode("bag_mlp", "classification")
set_model_mode("bag_mlp", "regression")
