#' @importFrom rlang enexprs

#' @importFrom stats binomial
logistic_reg_glm_constr <- 
  function(
    formula = missing_arg(),
    family = binomial,
    data = missing_arg(),
    weights = NULL,
    subset = missing_arg(),
    na.action = missing_arg(),
    start = NULL,
    etastart = missing_arg(),
    mustart = missing_arg(),
    offset = missing_arg(),
    control = list(),
    model = TRUE,
    method = "glm.fit",
    x = FALSE,
    y = TRUE,
    contrasts = NULL
  ) {
    libs <- "stats"
    interface <- "formula"
    protect = c("formula", "data", "weights", "family")   
    has_dots <- TRUE
    fit_name <- "glm"
    fit_args <- 
      enexprs(
        formula = formula,
        family = family,
        data = data,
        weights = weights,
        subset = subset,
        na.action = na.action,
        start = start,
        etastart = etastart,
        mustart = mustart,
        offset = offset,
        control = control,
        model = model,
        method = method,
        x = x,
        y = y,
        contrasts = contrasts
      )
    res <- 
      list(
        library = libs,
        interface = interface,
        protect = protect,
        has_dots = has_dots,
        fit = fit_args,
        fit_name = fit_name,
        fit_call = NULL
      )
    class(res) <- c("logistic_reg_constr")
    res
  }

logistic_reg_glmnet_constr <- 
  function(
    x = as.matrix(x),
    y = missing_arg(),
    family = "binomial",
    weights = NULL,
    offset = NULL,
    alpha = 1,
    nlambda = 100,
    lambda.min.ratio = ifelse(nrow(x) < ncol(x), 0.01, 1e-04),
    lambda = NULL,
    standardize = TRUE,
    intercept = TRUE,
    thresh = 1e-07,
    dfmax = ncol(x) + 1,
    pmax = min(dfmax * 2 + 20, ncol(x)),
    exclude = missing_arg(),
    penalty.factor = rep(1, ncol(x)),
    lower.limits = -Inf,
    upper.limits = Inf,
    maxit = 1e+05,
    type.gaussian = ifelse(ncol(x) < 500, "covariance", "naive"),
    type.logistic = c("Newton", "modified.Newton"),
    standardize.response = FALSE,
    type.multinomial = c("ungrouped", "grouped")
  ) {
    libs <- "glmnet"
    interface <- "data.frame"
    protect = c("x", "y", "weights", "family")   
    has_dots <- FALSE
    fit_name <- "glmnet"
    fit_args <- 
      enexprs(
        x = x,
        y = y,
        family = family,
        weights = weights,
        offset = offset,
        alpha = alpha,
        nlambda = nlambda,
        lambda.min.ratio = lambda.min.ratio,
        lambda = lambda,
        standardize = standardize,
        intercept = intercept,
        thresh = thresh,
        dfmax = dfmax,
        pmax = pmax,
        exclude = exclude,
        penalty.factor = penalty.factor,
        lower.limits = lower.limits,
        upper.limits = upper.limits,
        maxit = maxit,
        type.gaussian = type.gaussian,
        type.logistic = type.logistic,
        standardize.response = standardize.response,
        type.multinomial = type.multinomial
      )
    res <- 
      list(
        library = libs,
        interface = interface,
        protect = protect,
        has_dots = has_dots,
        fit = fit_args,
        fit_name = fit_name,
        fit_call = NULL
      )
    class(res) <- c("logistic_reg_constr")
    res
  }

logistic_reg_stan_constr <- 
  function(
    formula = missing_arg(),
    family = binomial(),
    data = missing_arg(),
    weights = NULL,
    subset= missing_arg(),
    na.action = NULL,
    offset = NULL,
    model = TRUE,
    x = FALSE,
    y = TRUE,
    contrasts = NULL,
    prior = normal(),
    prior_intercept = normal(),
    prior_aux = exponential(),
    prior_PD = FALSE,
    algorithm = c("sampling", "optimizing", "meanfield", "fullrank"),
    adapt_delta = NULL,
    QR = FALSE,
    sparse = FALSE
) {
    libs <- "rstanarm"
    interface <- "formula"
    protect = c("formula", "data", "weights", "family")   
    has_dots <- TRUE
    fit_name <- "stan_glm"
    fit_args <- 
      enexprs(
        formula = formula,
        family = family,
        data = data,
        weights = weights,
        subset = subset,
        na.action = na.action,
        offset = offset,
        model = model,
        x = x,
        y = y,
        contrasts = contrasts,
        prior = prior,
        prior_intercept = prior_intercept,
        prior_aux = prior_aux,
        prior_PD = prior_PD,
        algorithm = algorithm,
        adapt_delta = adapt_delta,
        QR = QR,
        sparse = sparse
      )
    res <- 
      list(
        library = libs,
        interface = interface,
        protect = protect,
        has_dots = has_dots,
        fit = fit_args,
        fit_name = fit_name,
        fit_call = NULL
      )
    class(res) <- c("logistic_reg_constr")
    res
  }


logistic_reg_spark_constr <- 
  function(
    x = missing_arg(),
    formula = missing_arg(),
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
    uid = random_string("logistic_regression_")
  ) {
  libs <- "sparklyr"
  interface <- "formula"
  protect = c("formula", "x", "weight_col")   
  has_dots <- TRUE
  fit_name <- "ml_logistic_regression"
  fit_args <- 
    enexprs(
      x = x,
      formula = formula,
      fit_intercept = fit_intercept,
      elastic_net_param = elastic_net_param,
      reg_param = reg_param,
      max_iter = max_iter,
      threshold = threshold,
      thresholds = thresholds,
      tol = tol,
      weight_col = weight_col,
      aggregation_depth = aggregation_depth,
      lower_bounds_on_coefficients = lower_bounds_on_coefficients,
      lower_bounds_on_intercepts = lower_bounds_on_intercepts,
      upper_bounds_on_coefficients = upper_bounds_on_coefficients,
      upper_bounds_on_intercepts = upper_bounds_on_intercepts,
      features_col = features_col,
      label_col = label_col,
      family = family,
      prediction_col = prediction_col,
      probability_col = probability_col,
      raw_prediction_col = raw_prediction_col,
      uid = uid
    )
  res <- 
    list(
      library = libs,
      interface = interface,
      protect = protect,
      has_dots = has_dots,
      fit = fit_args,
      fit_name = fit_name,
      fit_call = NULL
    )
  class(res) <- c("logistic_reg_constr")
  res
}

