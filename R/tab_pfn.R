#' TabPFN: prior data fitted networks
#'
#' @description
#' [tab_pfn()] uses a pre-trained deep learning network that emulates Bayesian
#' inference. The model was trained on a large number of simulated data sets
#' and an attention mechanism is use to make relevant predictions for specific
#' (i.e., real) data sets.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("tab_pfn")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param num_estimators An integer for the ensemble size. Default is `8L`.
#'
#' @param softmax_temperature An adjustment factor that is a divisor in the
#' exponents of the softmax function (see [tabpfn::tab_pfn()]). Defaults to 0.9.
#'
#' @param balance_probabilities A logical to adjust the prior probabilities in
#' cases where there is a class imbalance. Default is `FALSE`. Classification
#' only.
#'
#' @param average_before_softmax A logical. For cases where
#' `num_estimators > 1`, should the average be done before using the softmax
#' function or after? Default is `FALSE`.
#'
#' @param mode A single character value for the type of model.
#'  The possible values for this model are "classification" and "regression".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting. Possible engines are listed below. The default for this
#'  model is `"tabpfn"`.
#'
#' @templateVar modeltype tab_pfn
#' @template spec-details
#'
#' @details This function fits classification and regression models.
#'
#' @template spec-references
#'
#' @references
#'
#' [https://github.com/PriorLabs/TabPFN](https://github.com/PriorLabs/TabPFN)
#'
#' Hollmann, Noah, Samuel Müller, Lennart Purucker, Arjun Krishnakumar, Max
#' Körfer, Shi Bin Hoo, Robin Tibor Schirrmeister, and Frank Hutter.
#' "Accurate predictions on small data with a tabular foundation model."
#'  _Nature_ 637, no. 8045 (2025): 319-326.
#'
#' Hollmann, Noah, Samuel Müller, Katharina Eggensperger, and Frank Hutter.
#' "Tabpfn: A transformer that solves small tabular classification problems in
#' a second." _arXiv preprint_ arXiv:2207.01848 (2022).
#'
#' Müller, Samuel, Noah Hollmann, Sebastian Pineda Arango, Josif Grabocka, and
#' Frank Hutter. "Transformers can do Bayesian inference." _arXiv preprint_
#' arXiv:2112.10510 (2021).
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("tab_pfn")} [tabpfn::tab_pfn()]
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("tab_pfn")
#'
#' tab_pfn()
#' @export
tab_pfn <-
  function(
    mode = "unknown",
    engine = "tabpfn",
    num_estimators = NULL,
    softmax_temperature = NULL,
    balance_probabilities = NULL,
    average_before_softmax = NULL
  ) {
    args <- list(
      num_estimators = enquo(num_estimators),
      softmax_temperature = enquo(softmax_temperature),
      balance_probabilities = enquo(balance_probabilities),
      average_before_softmax = enquo(average_before_softmax)
    )

    new_model_spec(
      "tab_pfn",
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

#' @method update tab_pfn
#' @rdname parsnip_update
#' @export
update.tab_pfn <-
  function(
    object,
    parameters = NULL,
    num_estimators = NULL,
    softmax_temperature = NULL,
    balance_probabilities = NULL,
    average_before_softmax = NULL,
    fresh = FALSE,
    ...
  ) {
    args <- list(
      num_estimators = enquo(num_estimators),
      softmax_temperature = enquo(softmax_temperature),
      balance_probabilities = enquo(balance_probabilities),
      average_before_softmax = enquo(average_before_softmax)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "tab_pfn",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
check_args.tab_pfn <- function(object, call = rlang::caller_env()) {
  args <- lapply(object$args, rlang::eval_tidy)

  check_number_decimal(
    args$softmax_temperature,
    min = 0,
    allow_null = TRUE,
    call = call,
    arg = "softmax_temperature"
  )
  check_number_whole(
    args$num_estimators,
    min = 0,
    allow_null = TRUE,
    call = call,
    arg = "num_estimators"
  )
  check_logical(
    args$balance_probabilities,
    allow_null = TRUE,
    call = call,
    arg = "balance_probabilities"
  )
  check_logical(
    args$average_before_softmax,
    allow_null = TRUE,
    call = call,
    arg = "average_before_softmax"
  )
  invisible(object)
}

# ------------------------------------------------------------------------------

class_only <- function(x, object) {
  x[[".pred_class"]]
}

prob_only <- function(x, object) {
  x[, object$fit$levels]
}
