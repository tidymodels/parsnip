# Updating a model specification

If parameters of a model specification need to be modified,
[`update()`](https://rdrr.io/r/stats/update.html) can be used in lieu of
recreating the object from scratch.

## Usage

``` r
# S3 method for class 'bag_mars'
update(
  object,
  parameters = NULL,
  num_terms = NULL,
  prod_degree = NULL,
  prune_method = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'bag_mlp'
update(
  object,
  parameters = NULL,
  hidden_units = NULL,
  penalty = NULL,
  epochs = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'bag_tree'
update(
  object,
  parameters = NULL,
  cost_complexity = NULL,
  tree_depth = NULL,
  min_n = NULL,
  class_cost = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'bart'
update(
  object,
  parameters = NULL,
  trees = NULL,
  prior_terminal_node_coef = NULL,
  prior_terminal_node_expo = NULL,
  prior_outcome_range = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'boost_tree'
update(
  object,
  parameters = NULL,
  mtry = NULL,
  trees = NULL,
  min_n = NULL,
  tree_depth = NULL,
  learn_rate = NULL,
  loss_reduction = NULL,
  sample_size = NULL,
  stop_iter = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'C5_rules'
update(
  object,
  parameters = NULL,
  trees = NULL,
  min_n = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'cubist_rules'
update(
  object,
  parameters = NULL,
  committees = NULL,
  neighbors = NULL,
  max_rules = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'decision_tree'
update(
  object,
  parameters = NULL,
  cost_complexity = NULL,
  tree_depth = NULL,
  min_n = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'discrim_flexible'
update(
  object,
  num_terms = NULL,
  prod_degree = NULL,
  prune_method = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'discrim_linear'
update(
  object,
  penalty = NULL,
  regularization_method = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'discrim_quad'
update(object, regularization_method = NULL, fresh = FALSE, ...)

# S3 method for class 'discrim_regularized'
update(
  object,
  frac_common_cov = NULL,
  frac_identity = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'gen_additive_mod'
update(
  object,
  select_features = NULL,
  adjust_deg_free = NULL,
  parameters = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'linear_reg'
update(
  object,
  parameters = NULL,
  penalty = NULL,
  mixture = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'logistic_reg'
update(
  object,
  parameters = NULL,
  penalty = NULL,
  mixture = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'mars'
update(
  object,
  parameters = NULL,
  num_terms = NULL,
  prod_degree = NULL,
  prune_method = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'mlp'
update(
  object,
  parameters = NULL,
  hidden_units = NULL,
  penalty = NULL,
  dropout = NULL,
  epochs = NULL,
  activation = NULL,
  learn_rate = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'multinom_reg'
update(
  object,
  parameters = NULL,
  penalty = NULL,
  mixture = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'naive_Bayes'
update(object, smoothness = NULL, Laplace = NULL, fresh = FALSE, ...)

# S3 method for class 'nearest_neighbor'
update(
  object,
  parameters = NULL,
  neighbors = NULL,
  weight_func = NULL,
  dist_power = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'pls'
update(
  object,
  parameters = NULL,
  predictor_prop = NULL,
  num_comp = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'poisson_reg'
update(
  object,
  parameters = NULL,
  penalty = NULL,
  mixture = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'proportional_hazards'
update(
  object,
  parameters = NULL,
  penalty = NULL,
  mixture = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'rand_forest'
update(
  object,
  parameters = NULL,
  mtry = NULL,
  trees = NULL,
  min_n = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'rule_fit'
update(
  object,
  parameters = NULL,
  mtry = NULL,
  trees = NULL,
  min_n = NULL,
  tree_depth = NULL,
  learn_rate = NULL,
  loss_reduction = NULL,
  sample_size = NULL,
  penalty = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'surv_reg'
update(object, parameters = NULL, dist = NULL, fresh = FALSE, ...)

# S3 method for class 'survival_reg'
update(object, parameters = NULL, dist = NULL, fresh = FALSE, ...)

# S3 method for class 'svm_linear'
update(
  object,
  parameters = NULL,
  cost = NULL,
  margin = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'svm_poly'
update(
  object,
  parameters = NULL,
  cost = NULL,
  degree = NULL,
  scale_factor = NULL,
  margin = NULL,
  fresh = FALSE,
  ...
)

# S3 method for class 'svm_rbf'
update(
  object,
  parameters = NULL,
  cost = NULL,
  rbf_sigma = NULL,
  margin = NULL,
  fresh = FALSE,
  ...
)
```

## Arguments

- object:

  A [model
  specification](https://parsnip.tidymodels.org/reference/model_spec.md).

- parameters:

  A 1-row tibble or named list with *main* parameters to update. Use
  **either** `parameters` **or** the main arguments directly when
  updating. If the main arguments are used, these will supersede the
  values in `parameters`. Also, using engine arguments in this object
  will result in an error.

- num_terms:

  The number of features that will be retained in the final model,
  including the intercept.

- prod_degree:

  The highest possible interaction degree.

- prune_method:

  The pruning method.

- fresh:

  A logical for whether the arguments should be modified in-place or
  replaced wholesale.

- ...:

  Not used for [`update()`](https://rdrr.io/r/stats/update.html).

- hidden_units:

  An integer for the number of units in the hidden model.

- penalty:

  An non-negative number representing the amount of regularization used
  by some of the engines.

- epochs:

  An integer for the number of training iterations.

- cost_complexity:

  A positive number for the the cost/complexity parameter (a.k.a. `Cp`)
  used by CART models (specific engines only).

- tree_depth:

  An integer for maximum depth of the tree.

- min_n:

  An integer for the minimum number of data points in a node that are
  required for the node to be split further.

- class_cost:

  A non-negative scalar for a class cost (where a cost of 1 means no
  extra cost). This is useful for when the first level of the outcome
  factor is the minority class. If this is not the case, values between
  zero and one can be used to bias to the second level of the factor.

- trees:

  An integer for the number of trees contained in the ensemble.

- prior_terminal_node_coef:

  A coefficient for the prior probability that a node is a terminal
  node.

- prior_terminal_node_expo:

  An exponent in the prior probability that a node is a terminal node.

- prior_outcome_range:

  A positive value that defines the width of a prior that the predicted
  outcome is within a certain range. For regression it is related to the
  observed range of the data; the prior is the number of standard
  deviations of a Gaussian distribution defined by the observed range of
  the data. For classification, it is defined as the range of +/-3
  (assumed to be on the logit scale). The default value is 2.

- mtry:

  A number for the number (or proportion) of predictors that will be
  randomly sampled at each split when creating the tree models (specific
  engines only).

- learn_rate:

  A number for the rate at which the boosting algorithm adapts from
  iteration-to-iteration (specific engines only). This is sometimes
  referred to as the shrinkage parameter.

- loss_reduction:

  A number for the reduction in the loss function required to split
  further (specific engines only).

- sample_size:

  A number for the number (or proportion) of data that is exposed to the
  fitting routine. For `xgboost`, the sampling is done at each iteration
  while `C5.0` samples once during training.

- stop_iter:

  The number of iterations without improvement before stopping (specific
  engines only).

- committees:

  A non-negative integer (no greater than 100) for the number of members
  of the ensemble.

- neighbors:

  An integer between zero and nine for the number of training set
  instances that are used to adjust the model-based prediction.

- max_rules:

  The largest number of rules.

- regularization_method:

  A character string for the type of regularized estimation. Possible
  values are: "`diagonal`", "`min_distance`", "`shrink_cov`", and
  "`shrink_mean`" (`sparsediscrim` engine only).

- frac_common_cov, frac_identity:

  Numeric values between zero and one.

- select_features:

  `TRUE` or `FALSE.` If `TRUE`, the model has the ability to eliminate a
  predictor (via penalization). Increasing `adjust_deg_free` will
  increase the likelihood of removing predictors.

- adjust_deg_free:

  If `select_features = TRUE`, then acts as a multiplier for smoothness.
  Increase this beyond 1 to produce smoother models.

- mixture:

  A number between zero and one (inclusive) denoting the proportion of
  L1 regularization (i.e. lasso) in the model.

  - `mixture = 1` specifies a pure lasso model,

  - `mixture = 0` specifies a ridge regression model, and

  - `0 < mixture < 1` specifies an elastic net model, interpolating
    lasso and ridge.

  Available for specific engines only.

- dropout:

  A number between 0 (inclusive) and 1 denoting the proportion of model
  parameters randomly set to zero during model training.

- activation:

  A single character string denoting the type of relationship between
  the original predictors and the hidden unit layer. The activation
  function between the hidden and output layers is automatically set to
  either "linear" or "softmax" depending on the type of outcome.
  Possible values depend on the engine being used.

- smoothness:

  An non-negative number representing the the relative smoothness of the
  class boundary. Smaller examples result in model flexible boundaries
  and larger values generate class boundaries that are less adaptable

- Laplace:

  A non-negative value for the Laplace correction to smoothing
  low-frequency counts.

- weight_func:

  A *single* character for the type of kernel function used to weight
  distances between samples. Valid choices are: `"rectangular"`,
  `"triangular"`, `"epanechnikov"`, `"biweight"`, `"triweight"`,
  `"cos"`, `"inv"`, `"gaussian"`, `"rank"`, or `"optimal"`.

- dist_power:

  A single number for the parameter used in calculating Minkowski
  distance.

- predictor_prop:

  The maximum proportion of original predictors that can have *non-zero*
  coefficients for each PLS component (via regularization). This value
  is used for all PLS components for X.

- num_comp:

  The number of PLS components to retain.

- dist:

  A character string for the probability distribution of the outcome.
  The default is "weibull".

- cost:

  A positive number for the cost of predicting a sample within or on the
  wrong side of the margin

- margin:

  A positive number for the epsilon in the SVM insensitive loss function
  (regression only)

- degree:

  A positive number for polynomial degree.

- scale_factor:

  A positive number for the polynomial scaling factor.

- rbf_sigma:

  A positive number for radial basis function.

## Value

An updated model specification.

## Examples

``` r
# ------------------------------------------------------------------------------

model <- C5_rules(trees = 10, min_n = 2)
model
#> ! parsnip could not locate an implementation for `C5_rules` model
#>   specifications.
#> ℹ The parsnip extension package rules implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> C5.0 Model Specification (classification)
#> 
#> Main Arguments:
#>   trees = 10
#>   min_n = 2
#> 
#> Computational engine: C5.0 
#> 
update(model, trees = 1)
#> ! parsnip could not locate an implementation for `C5_rules` model
#>   specifications.
#> ℹ The parsnip extension package rules implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> C5.0 Model Specification (classification)
#> 
#> Main Arguments:
#>   trees = 1
#>   min_n = 2
#> 
#> Computational engine: C5.0 
#> 
update(model, trees = 1, fresh = TRUE)
#> ! parsnip could not locate an implementation for `C5_rules` model
#>   specifications.
#> ℹ The parsnip extension package rules implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> C5.0 Model Specification (classification)
#> 
#> Main Arguments:
#>   trees = 1
#> 
#> Computational engine: C5.0 
#> 

# ------------------------------------------------------------------------------

model <- cubist_rules(committees = 10, neighbors = 2)
model
#> ! parsnip could not locate an implementation for `cubist_rules` model
#>   specifications.
#> ℹ The parsnip extension package rules implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> Cubist Model Specification (regression)
#> 
#> Main Arguments:
#>   committees = 10
#>   neighbors = 2
#> 
#> Computational engine: Cubist 
#> 
update(model, committees = 1)
#> ! parsnip could not locate an implementation for `cubist_rules` model
#>   specifications.
#> ℹ The parsnip extension package rules implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> Cubist Model Specification (regression)
#> 
#> Main Arguments:
#>   committees = 1
#>   neighbors = 2
#> 
#> Computational engine: Cubist 
#> 
update(model, committees = 1, fresh = TRUE)
#> ! parsnip could not locate an implementation for `cubist_rules` model
#>   specifications.
#> ℹ The parsnip extension package rules implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> Cubist Model Specification (regression)
#> 
#> Main Arguments:
#>   committees = 1
#> 
#> Computational engine: Cubist 
#> 
model <- pls(predictor_prop =  0.1)
model
#> ! parsnip could not locate an implementation for `pls` model
#>   specifications.
#> ℹ The parsnip extension package plsmod implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> PLS Model Specification (unknown mode)
#> 
#> Main Arguments:
#>   predictor_prop = 0.1
#> 
#> Computational engine: mixOmics 
#> 
update(model, predictor_prop = 1)
#> ! parsnip could not locate an implementation for `pls` model
#>   specifications.
#> ℹ The parsnip extension package plsmod implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> PLS Model Specification (unknown mode)
#> 
#> Main Arguments:
#>   predictor_prop = 1
#> 
#> Computational engine: mixOmics 
#> 
update(model, predictor_prop = 1, fresh = TRUE)
#> ! parsnip could not locate an implementation for `pls` model
#>   specifications.
#> ℹ The parsnip extension package plsmod implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> PLS Model Specification (unknown mode)
#> 
#> Main Arguments:
#>   predictor_prop = 1
#> 
#> Computational engine: mixOmics 
#> 
# ------------------------------------------------------------------------------

model <- rule_fit(trees = 10, min_n = 2)
model
#> ! parsnip could not locate an implementation for `rule_fit` model
#>   specifications.
#> ℹ The parsnip extension packages agua and rules implement support for
#>   this specification.
#> ℹ Please install (if needed) and load to continue.
#> RuleFit Model Specification (unknown mode)
#> 
#> Main Arguments:
#>   trees = 10
#>   min_n = 2
#> 
#> Computational engine: xrf 
#> 
update(model, trees = 1)
#> ! parsnip could not locate an implementation for `rule_fit` model
#>   specifications.
#> ℹ The parsnip extension packages agua and rules implement support for
#>   this specification.
#> ℹ Please install (if needed) and load to continue.
#> RuleFit Model Specification (unknown mode)
#> 
#> Main Arguments:
#>   trees = 1
#>   min_n = 2
#> 
#> Computational engine: xrf 
#> 
update(model, trees = 1, fresh = TRUE)
#> ! parsnip could not locate an implementation for `rule_fit` model
#>   specifications.
#> ℹ The parsnip extension packages agua and rules implement support for
#>   this specification.
#> ℹ Please install (if needed) and load to continue.
#> RuleFit Model Specification (unknown mode)
#> 
#> Main Arguments:
#>   trees = 1
#> 
#> Computational engine: xrf 
#> 
model <- boost_tree(mtry = 10, min_n = 3)
model
#> Boosted Tree Model Specification (unknown mode)
#> 
#> Main Arguments:
#>   mtry = 10
#>   min_n = 3
#> 
#> Computational engine: xgboost 
#> 
update(model, mtry = 1)
#> Boosted Tree Model Specification (unknown mode)
#> 
#> Main Arguments:
#>   mtry = 1
#>   min_n = 3
#> 
#> Computational engine: xgboost 
#> 
update(model, mtry = 1, fresh = TRUE)
#> Boosted Tree Model Specification (unknown mode)
#> 
#> Main Arguments:
#>   mtry = 1
#> 
#> Computational engine: xgboost 
#> 

param_values <- tibble::tibble(mtry = 10, tree_depth = 5)

model |> update(param_values)
#> Boosted Tree Model Specification (unknown mode)
#> 
#> Main Arguments:
#>   mtry = 10
#>   min_n = 3
#>   tree_depth = 5
#> 
#> Computational engine: xgboost 
#> 
model |> update(param_values, mtry = 3)
#> Boosted Tree Model Specification (unknown mode)
#> 
#> Main Arguments:
#>   mtry = 10
#>   min_n = 3
#>   tree_depth = 5
#> 
#> Computational engine: xgboost 
#> 

param_values$verbose <- 0
# Fails due to engine argument
# model |> update(param_values)

model <- linear_reg(penalty = 10, mixture = 0.1)
model
#> Linear Regression Model Specification (regression)
#> 
#> Main Arguments:
#>   penalty = 10
#>   mixture = 0.1
#> 
#> Computational engine: lm 
#> 
update(model, penalty = 1)
#> Linear Regression Model Specification (regression)
#> 
#> Main Arguments:
#>   penalty = 1
#>   mixture = 0.1
#> 
#> Computational engine: lm 
#> 
update(model, penalty = 1, fresh = TRUE)
#> Linear Regression Model Specification (regression)
#> 
#> Main Arguments:
#>   penalty = 1
#> 
#> Computational engine: lm 
#> 
```
