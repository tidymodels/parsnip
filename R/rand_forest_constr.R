
rand_forest_arg_key <- data.frame(
  randomForest = c("mtry", "ntree", "nodesize"),
  ranger = c("mtry", "num.trees", "min.node.size"),
  spark = 
    c("feature_subset_strategy", "num_trees", "min_instances_per_node"),
  stringsAsFactors = FALSE,
  row.names =  c("mtry", "trees", "min_n")
)

rand_forest_modes <- c("classification", "regression", "unknown")

rand_forest_engines <- data.frame(
  ranger =       c(TRUE, TRUE, FALSE),
  randomForest = c(TRUE, TRUE, FALSE),
  spark =        c(TRUE, TRUE, FALSE),
  row.names =  c("classification", "regression", "unknown")
)

###################################################################

rand_forest_ranger_constr <- 
  function(
    formula = missing_arg(),
    data = missing_arg(),
    num.trees = 500,
    mtry =  NULL,
    importance = "none",
    write.forest = TRUE,
    probability = FALSE,
    min.node.size = NULL,
    replace = TRUE,
    sample.fraction = ifelse(replace, 1, 0.632),
    case.weights = NULL,
    splitrule = NULL,
    num.random.splits = 1,
    alpha = 0.5,
    minprop = 0.1,
    split.select.weights = NULL,
    always.split.variables = NULL,
    respect.unordered.factors = NULL,
    scale.permutation.importance = FALSE,
    keep.inbag = FALSE,
    holdout = FALSE,
    num.threads = NULL,
    save.memory = FALSE,
    verbose = FALSE,
    seed = sample.int(10^5, 1),
    dependent.variable.name = NULL,
    status.variable.name = NULL,
    classification = NULL
  ) {
    libs <- "ranger"
    interface <- "formula"
    protect = c("formula", "data", "case.weights")   
    has_dots <- FALSE
    fit_name <- "ranger"
    fit_args <- 
      enexprs(
        formula = formula,
        data = data,
        num.trees = num.trees,
        mtry = mtry,
        importance = importance,
        write.forest = write.forest,
        probability = probability,
        min.node.size = min.node.size,
        replace = replace,
        sample.fraction = sample.fraction,
        case.weights = case.weights,
        splitrule = splitrule,
        num.random.splits = num.random.splits,
        alpha = alpha,
        minprop = minprop,
        split.select.weights = split.select.weights,
        always.split.variables = always.split.variables,
        respect.unordered.factors = respect.unordered.factors,
        scale.permutation.importance = scale.permutation.importance,
        keep.inbag = keep.inbag,
        holdout = holdout,
        num.threads = num.threads,
        save.memory = save.memory,
        verbose = verbose,
        seed = seed,
        dependent.variable.name = dependent.variable.name,
        status.variable.name = status.variable.name,
        classification = classification
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
    class(res) <- c("rand_forest_constr")
    res
  }

rand_forest_randomForest_constr <- 
  function(
    x  = missing_arg(), 
    y = missing_arg(),  
    xtest = NULL, 
    ytest = NULL, 
    ntree = 500, 
    mtry = if (!is.null(y) && !is.factor(y))
      max(floor(ncol(x) / 3), 1)
    else
      floor(sqrt(ncol(x))),
    replace = TRUE,
    classwt = NULL,
    cutoff = missing_arg(),
    strata = missing_arg(),
    sampsize = if (replace)
      nrow(x)
    else
      ceiling(.632 * nrow(x)),
    nodesize = if (!is.null(y) && !is.factor(y))
      5
    else
      1,
    maxnodes = NULL,
    importance = FALSE,
    localImp = FALSE,
    nPerm = 1,
    proximity = missing_arg(),
    oob.prox = proximity,
    norm.votes = TRUE,
    do.trace = FALSE,
    keep.forest = !is.null(y) && is.null(xtest),
    corr.bias = FALSE,
    keep.inbag = FALSE
  ) 
  {
    libs <- "randomForest"
    interface <- "data.frame"
    protect = c("x", "y")
    has_dots <- TRUE
    fit_name <- "randomForest"
    fit_args <-
      enexprs(
        x = x,
        y = y,
        xtest = xtest,
        ytest = ytest,
        ntree = ntree,
        mtry = mtry,
        replace = replace,
        classwt = classwt,
        cutoff = cutoff,
        strata = strata,
        sampsize = sampsize,
        nodesize = nodesize,
        maxnodes = maxnodes,
        importance = importance,
        localImp = localImp,
        nPerm = nPerm,
        proximity = proximity,
        oob.prox = oob.prox,
        norm.votes = norm.votes,
        do.trace = do.trace,
        keep.forest = keep.forest,
        corr.bias = corr.bias,
        keep.inbag = keep.inbag
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
    class(res) <- c("rand_forest_constr")
    res  
  }


rand_forest_spark_constr <- 
  function(
    x = missing_arg(),
    formula = NULL,
    type = missing_arg(),
    features_col = missing_arg(),
    label_col = missing_arg(),
    prediction_col = "prediction",
    probability_col = "probability",
    raw_prediction_col = "rawPrediction",
    feature_subset_strategy = "auto",
    impurity = "auto",
    checkpoint_interval = 10L,
    max_bins = 32L,
    max_depth = 5L,
    num_trees = 20L,
    min_info_gain = 0,
    min_instances_per_node = 1L,
    subsampling_rate = 1,
    seed = NULL,
    thresholds = NULL,
    cache_node_ids = FALSE,
    max_memory_in_mb = 256L,
    uid = random_string("random_forest_"),
    response = NULL,
    features = NULL
  )
  {
    libs <- "sparklyr"
    interface <- "formula"
    protect = c("x", "features_col", "label_col", "type")
    has_dots <- TRUE
    fit_name <- "ml_random_forest"
    fit_args <-
      enexprs(
        x = x,
        formula = formula,
        type = type,
        features_col = features_col,
        label_col = label_col,
        prediction_col = prediction_col,
        probability_col = probability_col,
        raw_prediction_col = raw_prediction_col,
        feature_subset_strategy = feature_subset_strategy,
        impurity = impurity,
        checkpoint_interval = checkpoint_interval,
        max_bins = max_bins,
        max_depth = max_depth,
        num_trees = num_trees,
        min_info_gain = min_info_gain,
        min_instances_per_node = min_instances_per_node,
        subsampling_rate = subsampling_rate,
        seed = seed,
        thresholds = thresholds,
        cache_node_ids = cache_node_ids,
        max_memory_in_mb = max_memory_in_mb,
        uid = uid,
        response = response,
        features = features
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
    class(res) <- c("rand_forest_constr")
    res  
  }



