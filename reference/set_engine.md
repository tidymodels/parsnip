# Declare a computational engine and specific arguments

`set_engine()` is used to specify which package or system will be used
to fit the model, along with any arguments specific to that software.

## Usage

``` r
set_engine(object, engine, ...)
```

## Arguments

- object:

  A [model
  specification](https://parsnip.tidymodels.org/reference/model_spec.md).

- engine:

  A character string for the software that should be used to fit the
  model. This is highly dependent on the type of model (e.g. linear
  regression, random forest, etc.).

- ...:

  Any optional arguments associated with the chosen computational
  engine. These are captured as quosures and can be tuned with
  [`tune()`](https://hardhat.tidymodels.org/reference/tune.html).

## Value

An updated model specification.

## Details

In parsnip,

- the model **type** differentiates basic modeling approaches, such as
  random forests, logistic regression, linear support vector machines,
  etc.,

- the **mode** denotes in what kind of modeling context it will be used
  (most commonly, classification or regression), and

- the computational **engine** indicates how the model is fit, such as
  with a specific R package implementation or even methods outside of R
  like Keras or Stan.

Use
[`show_engines()`](https://parsnip.tidymodels.org/reference/show_engines.md)
to get a list of possible engines for the model of interest.

Modeling functions in parsnip separate model arguments into two
categories:

- *Main arguments* are more commonly used and tend to be available
  across engines. These names are standardized to work with different
  engines in a consistent way, so you can use the parsnip main argument
  `trees`, instead of the heterogeneous arguments for this parameter
  from ranger and randomForest packages (`num.trees` and `ntree`,
  respectively). Set these in your model type function, like
  `rand_forest(trees = 2000)`.

- *Engine arguments* are either specific to a particular engine or used
  more rarely; there is no change for these argument names from the
  underlying engine. The `...` argument of `set_engine()` allows any
  engine-specific argument to be passed directly to the engine fitting
  function, like `set_engine("ranger", importance = "permutation")`.

## Examples

``` r
# First, set main arguments using the standardized names
logistic_reg(penalty = 0.01, mixture = 1/3) |>
  # Now specify how you want to fit the model with another argument
  set_engine("glmnet", nlambda = 10) |>
  translate()
#> Logistic Regression Model Specification (classification)
#> 
#> Main Arguments:
#>   penalty = 0.01
#>   mixture = 1/3
#> 
#> Engine-Specific Arguments:
#>   nlambda = 10
#> 
#> Computational engine: glmnet 
#> 
#> Model fit template:
#> glmnet::glmnet(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
#>     alpha = 1/3, nlambda = 10, family = "binomial")

# Many models have possible engine-specific arguments
decision_tree(tree_depth = 5) |>
  set_engine("rpart", parms = list(prior = c(.65,.35))) |>
  set_mode("classification") |>
  translate()
#> Decision Tree Model Specification (classification)
#> 
#> Main Arguments:
#>   tree_depth = 5
#> 
#> Engine-Specific Arguments:
#>   parms = list(prior = c(0.65, 0.35))
#> 
#> Computational engine: rpart 
#> 
#> Model fit template:
#> rpart::rpart(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
#>     maxdepth = 5, parms = list(prior = c(0.65, 0.35)))
```
