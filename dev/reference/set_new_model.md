# Tools to Register Models

These functions are similar to constructors and can be used to validate
that there are no conflicts with the underlying model structures used by
the package.

## Usage

``` r
set_new_model(model)

set_model_mode(model, mode)

set_model_engine(model, mode, eng)

set_model_arg(model, eng, parsnip, original, func, has_submodel)

set_dependency(model, eng, pkg = "parsnip", mode = NULL)

get_dependency(model)

set_fit(model, mode, eng, value)

get_fit(model)

set_pred(model, mode, eng, type, value)

get_pred_type(model, type)

show_model_info(model)

pred_value_template(pre = NULL, post = NULL, func, ...)

set_encoding(model, mode, eng, options)

get_encoding(model)
```

## Arguments

- model:

  A single character string for the model type (e.g. `"rand_forest"`,
  etc).

- mode:

  A single character string for the model mode (e.g. "regression").

- eng:

  A single character string for the model engine.

- parsnip:

  A single character string for the "harmonized" argument name that
  parsnip exposes.

- original:

  A single character string for the argument name that underlying model
  function uses.

- func:

  A named character vector that describes how to call a function. `func`
  should have elements `pkg` and `fun`. The former is optional but is
  recommended and the latter is required. For example,
  `c(pkg = "stats", fun = "lm")` would be used to invoke the usual
  linear regression function. In some cases, it is helpful to use
  `c(fun = "predict")` when using a package's `predict` method.

- has_submodel:

  A single logical for whether the argument can make predictions on
  multiple submodels at once.

- pkg:

  An options character string for a package name.

- value:

  A list that conforms to the `fit_obj` or `pred_obj` description below,
  depending on context.

- type:

  A single character value for the type of prediction. Possible values
  are: `class`, `conf_int`, `numeric`, `pred_int`, `prob`, `quantile`,
  and `raw`.

- pre, post:

  Optional functions for pre- and post-processing of prediction results.

- ...:

  Optional arguments that should be passed into the `args` slot for
  prediction objects.

- options:

  A list of options for engine-specific preprocessing encodings. See
  Details below.

## Details

These functions are available for users to add their own models or
engines (in a package or otherwise) so that they can be accessed using
parsnip. This is more thoroughly documented on the package web site (see
references below).

In short, `parsnip` stores an environment object that contains all of
the information and code about how models are used (e.g. fitting,
predicting, etc). These functions can be used to add models to that
environment as well as helper functions that can be used to makes sure
that the model data is in the right format.

`check_model_exists()` checks the model value and ensures that the model
has already been registered. `check_model_doesnt_exist()` checks the
model value and also checks to see if it is novel in the environment.

The options for engine-specific encodings dictate how the predictors
should be handled. These options ensure that the data that `parsnip`
gives to the underlying model allows for a model fit that is as similar
as possible to what it would have produced directly.

For example, if [`fit()`](https://generics.r-lib.org/reference/fit.html)
is used to fit a model that does not have a formula interface, typically
some predictor preprocessing must be conducted. `glmnet` is a good
example of this.

There are four options that can be used for the encodings:

`predictor_indicators` describes whether and how to create
indicator/dummy variables from factor predictors. There are three
options: `"none"` (do not expand factor predictors), `"traditional"`
(apply the standard
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
encodings), and `"one_hot"` (create the complete set including the
baseline level for all factors). This encoding only affects cases when
[`fit.model_spec()`](https://parsnip.tidymodels.org/dev/reference/fit.md)
is used and the underlying model has an x/y interface.

Another option is `compute_intercept`; this controls whether
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) should
include the intercept in its formula. This affects more than the
inclusion of an intercept column. With an intercept,
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) computes
dummy variables for all but one factor levels. Without an intercept,
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) computes a
full set of indicators for the *first* factor variable, but an
incomplete set for the remainder.

Next, the option `remove_intercept` will remove the intercept column
*after* [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) is
finished. This can be useful if the model function (e.g.
[`lm()`](https://rdrr.io/r/stats/lm.html)) automatically generates an
intercept.

Finally, `allow_sparse_x` specifies whether the model function can
natively accommodate a sparse matrix representation for predictors
during fitting and tuning.

## References

"How to build a parsnip model"
<https://www.tidymodels.org/learn/develop/models/>

## Examples

``` r
# set_new_model("shallow_learning_model")

# Show the information about a model:
show_model_info("rand_forest")
#> Information for `rand_forest`
#>  modes: unknown, classification, regression, censored regression, quantile regression 
#> 
#>  engines: 
#>    classification:      grf¹, randomForest, ranger¹, spark
#>    quantile regression: grf
#>    regression:          grf¹, randomForest, ranger¹, spark
#> 
#> ¹The model can use case weights.
#> 
#>  arguments: 
#>    ranger:       
#>       mtry  --> mtry
#>       trees --> num.trees
#>       min_n --> min.node.size
#>    randomForest: 
#>       mtry  --> mtry
#>       trees --> ntree
#>       min_n --> nodesize
#>    spark:        
#>       mtry  --> feature_subset_strategy
#>       trees --> num_trees
#>       min_n --> min_instances_per_node
#>    grf:          
#>       mtry  --> mtry
#>       trees --> num.trees
#>       min_n --> min.node.size
#> 
#>  fit modules:
#>          engine                mode
#>          ranger      classification
#>          ranger          regression
#>    randomForest      classification
#>    randomForest          regression
#>           spark      classification
#>           spark          regression
#>             grf      classification
#>             grf          regression
#>             grf quantile regression
#> 
#>  prediction modules:
#>                   mode       engine                    methods
#>         classification          grf      class, conf_int, prob
#>         classification randomForest           class, prob, raw
#>         classification       ranger class, conf_int, prob, raw
#>         classification        spark                class, prob
#>    quantile regression          grf                   quantile
#>             regression          grf          conf_int, numeric
#>             regression randomForest               numeric, raw
#>             regression       ranger     conf_int, numeric, raw
#>             regression        spark                    numeric
#> 
```
