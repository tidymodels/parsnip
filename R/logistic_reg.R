#' General Interface for Logistic Regression Models
#' 
#' `logistic_reg` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R, Stan, or via Spark. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{link}: The link function.
#'   \item \code{regularization}: The total amount of regularization
#'  in the model. Note that this must be zero for some engines.
#'   \item \code{mixture}: The proportion of L2 regularization in
#'  the model. Note that this will be ignored for some engines.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using the `engine_args` argument. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions.
#' 
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `logistic_reg`,the
#'  mode will always be "classification".
#' 
#' The model can be created using the [fit()] function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"glm"` or `"glmnet"` 
#' \item \pkg{Stan}:  `"rstanarm"` 
#' \item \pkg{Spark}: `"spark"`
#' }
#' @export
#' @rdname logistic_reg
#' @importFrom rlang expr enquo missing_arg
#' @importFrom purrr map_lgl
#' @seealso [varying()], [fit()]
#' @examples 
#' logistic_reg()
#' 
#' # Parameters can be represented by a placeholder:
#' logistic_reg(link = "probit", regularization = varying())

logistic_reg <- function (mode, ...)
  UseMethod("logistic_reg")

#' @rdname logistic_reg
#' @export
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "classification".
#' @param engine_args A named list of arguments to be used by the
#'  underlying models (e.g., `stats::glm`,
#'  `rstanarm::stan_glm`, etc.). These are not evaluated
#'  until the model is fit and will be substituted into the model
#'  fit expression.
#' @param link A character string for the link function. Possible
#'  values are "logit", "probit", "cauchit", "log" and "cloglog".
#' @param regularization An non-negative number representing the
#'  total amount of regularization.
#' @param mixture A number between zero and one (inclusive) that
#'  represents the proportion of regularization that is used for the
#'  L2 penalty (i.e. weight decay, or ridge regression) versus L1
#'  (the lasso).
#' @param ... Used for S3 method consistency. Any arguments passed to
#'  the ellipses will result in an error. Use `engine_args` instead.


logistic_reg.default <-
  function(mode = "classification",
           link = NULL,
           regularization = NULL,
           mixture = NULL,
           engine_args = list(),
           ...) {
    check_empty_ellipse(...)
    if (!(mode %in% logistic_reg_modes))
      stop(
        "`mode` should be one of: ",
        paste0("'", logistic_reg_modes, "'", collapse = ", "),
        call. = FALSE
      )
    
    args <- list(
      link = rlang::enquo(link),
      regularization = rlang::enquo(regularization),
      mixture = rlang::enquo(mixture)
    )
    
    others <- parse_engine_options(rlang::enquo(engine_args))
    
    # write a constructor function
    out <- list(
      args = args,
      others = others,
      mode = mode,
      method = NULL,
      engine = NULL
    )
    class(out) <- make_classes("logistic_reg", mode)
    out
  }

#' @export
print.logistic_reg <- function(x, ...) {
  cat("Logistic Regression Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)
  invisible(x)
}

###################################################################

logistic_reg_glm_classification <- function () {
  libs <- "stats"
  interface <- "formula"
  protect = c("glm", "formula", "data", "weights")  
  fit <- 
    quote(
      glm(
        formula = missing_arg(),
        family = binomial(),
        data = missing_arg(),
        weights = missing_arg(),
        subset = missing_arg(),
        na.action = missing_arg(),
        start = NULL,
        etastart = missing_arg(),
        mustart = missing_arg(),
        offset = missing_arg(),
        control = list(...),
        model = TRUE,
        method = "glm.fit",
        x = FALSE,
        y = TRUE,
        contrasts = NULL,
        ... = missing_arg()
      )
    ) 
  list(library = libs, interface = interface, fit = fit, protect = protect)
}

logistic_reg_glmnet_classification <- function () {
  libs <- "glmnet"
  interface <- "data.frame"
  protect = c("glmnet", "x", "y", "weights")
  fit <- 
    quote(
      glmnet(
        x = x, 
        y = y, 
        family = "binomial", 
        weights = missing_arg(), 
        offset = NULL,
        alpha = 1, 
        nlambda = 100, 
        lambda.min.ratio = ifelse(nobs < nvars, 0.01, 1e-04), 
        lambda = NULL, 
        standardize = TRUE,
        intercept = TRUE, 
        thresh = 1e-07, 
        dfmax = nvars + 1, 
        pmax = min(dfmax * 2 + 20, nvars), 
        exclude = missing_arg(), 
        penalty.factor = rep(1, nvars),
        lower.limits = -Inf, 
        upper.limits = Inf,
        maxit = 1e+05, 
        type.gaussian = ifelse(nvars < 500, "covariance", "naive"), 
        type.logistic = c("Newton", "modified.Newton"), 
        standardize.response = FALSE, 
        type.multinomial = c("ungrouped", "grouped")
      )
    ) 
  list(library = libs, interface = interface, fit = fit, protect = protect)
}

logistic_reg_spark_classification <- function () {
  libs <- "sparklyr"
  interface <- "data.frame" 
  protect = c("ml_logistic_regression", "x", "formula", "label_col", "features_col")
  fit <- 
    quote(
      ml_logistic_regression(
        x = x,
        formula = NULL,
        fit_intercept = TRUE,
        elastic_net_param = 0,
        reg_param = 0,
        max_iter = 100L,
        threshold = 0.5,
        thresholds = NULL,
        tol = 1e-06,
        weight_col = NULL,
        aggregation_depth = 2L,
        lower_bounds_on_coefficients = NULL,
        lower_bounds_on_intercepts = NULL,
        upper_bounds_on_coefficients = NULL,
        upper_bounds_on_intercepts = NULL,
        features_col = "features",
        label_col = "label",
        family = "auto",
        prediction_col = "prediction",
        probability_col = "probability",
        raw_prediction_col = "rawPrediction",
        uid = random_string("logistic_regression_"),
        ... = missing_arg()
      )
    ) 
  list(library = libs, interface = interface, fit = fit, protect = protect)
}

logistic_reg_stan_glm_classification <- function () {
  libs <- "rstanarm"
  interface <- "formula" 
  protect = c("stan_glm", "formula", "data", "weights")  
  fit <- 
    quote(
      stan_glm(
        formula = missing_arg(),
        family = binomial(),
        data = missing_arg(),
        weights = missing_arg(),
        subset = missing_arg(),
        na.action = NULL,
        offset = NULL,
        model = TRUE,
        x = FALSE,
        y = TRUE,
        contrasts = NULL,
        ... = missing_arg(),
        prior = normal(),
        prior_intercept = normal(),
        prior_aux = exponential(),
        prior_PD = FALSE,
        algorithm = c("sampling", "optimizing", "meanfield", "fullrank"),
        adapt_delta = NULL,
        QR = FALSE,
        sparse = FALSE
      )
    ) 
  list(library = libs, interface = interface, fit = fit, protect = protect)
}

#' @importFrom rlang quos
#' @export
finalize.logistic_reg <- function(x, engine = NULL, ...) {
  check_empty_ellipse(...)
  
  x$engine <- engine
  x <- check_engine(x)
  
  # exceptions and error trapping here
  if(engine %in% c("glm", "stan_glm") & !is.null(x$args$regularization)) {
    warning("The argument `regularization` cannot be used with this engine. ",
            "The value will be set to NULL")
    x$args$regularization <- quos(NULL)
  }
  if(engine %in% c("glm", "stan_glm") & !is.null(x$args$mixture)) {
    warning("The argument `mixture` cannot be used with this engine. ",
            "The value will be set to NULL")
    x$args$mixture <- quos(NULL)
  }
  
  x$method <- get_model_objects(x, x$engine)()
  real_args <- deharmonize(x$args, logistic_reg_arg_key, x$engine)
  
  # replace default args with user-specified
  x$method$fit <-
    sub_arg_values(x$method$fit, real_args, ignore = x$method$protect)
  
  if (length(x$others) > 0) {
    protected <- names(x$others) %in% x$method$protect
    if (any(protected)) {
      warning(
        "The following options cannot be changed at this time ",
        "and were removed: ",
        paste0("`", names(x$others)[protected], "`", collapse = ", "),
        call. = FALSE
      )
      x$others <- x$others[-which(protected)]
    }
  }
  if (length(x$others) > 0)
    x$method$fit <- sub_arg_values(x$method$fit, x$others, ignore = x$method$protect)
  
  # remove NULL and unmodified argument values
  modifed_args <- names(real_args)[!vapply(real_args, null_value, lgl(1))]
  x$method$fit <- prune_expr(x$method$fit, x$method$protect, c(modifed_args, names(x$others)))
  x
}

###################################################################

#' @export
update.logistic_reg <-
  function(object,
           link = NULL, regularization = NULL, mixture = NULL,
           engine_args = list(),
           fresh = FALSE,
           ...) {
    check_empty_ellipse(...)
    
    args <- list(
      link = rlang::enquo(link),
      regularization = rlang::enquo(regularization),
      mixture = rlang::enquo(mixture)
    )
    if (fresh) {
      object$args <- args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }
    
    others <- parse_engine_options(rlang::enquo(engine_args))
    if (length(others) > 0) {
      if (fresh)
        object$others <- others
      else
        object$others[names(others)] <- others
    }
    
    object
  }


###################################################################

logistic_reg_arg_key <- data.frame(
  glm =      c("link",          NA,                  NA),
  glmnet =   c(    NA,    "lambda",             "alpha"),
  spark =    c(    NA, "reg_param", "elastic_net_param"),
  stan_glm = c("link",          NA,                  NA),
  stringsAsFactors = FALSE,
  row.names =  c("link", "regularization", "mixture")
)

logistic_reg_modes <- "classification"

logistic_reg_engines <- data.frame(
  glm =      TRUE,
  glmnet =   TRUE,
  spark =    TRUE,
  stan_glm = TRUE,  
  row.names =  c("classification")
)

