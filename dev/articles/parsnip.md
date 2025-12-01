# Introduction to parsnip

This package provides functions and methods to create and manipulate
functions commonly used during modeling (e.g. fitting the model, making
predictions, etc). It allows the user to manipulate how the same type of
model can be created from different sources.

### Motivation

Modeling functions across different R packages can have very different
interfaces. If you would like to try different approaches, there is a
lot of syntactical minutiae to remember. The problem worsens when you
move in-between platforms (e.g. doing a logistic regression in R’s `glm`
versus Spark’s implementation).

parsnip tries to solve this by providing similar interfaces to models.
For example, if you are fitting a random forest model and would like to
adjust the number of trees in the forest there are different argument
names to remember:

- [`randomForest::randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  uses `ntree`,
- [`ranger::ranger`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  uses `num.trees`,  
- Spark’s
  [`sparklyr::ml_random_forest`](https://rdrr.io/pkg/sparklyr/man/ml_random_forest.html)
  uses `num_trees`.

Rather than remembering these values, a common interface to these models
can be used with

``` r
library(parsnip)
rf_mod <- rand_forest(trees = 2000)
```

The package makes the translation between `trees` and the real names in
each of the implementations.

Some terminology:

- The **model type** differentiates models. Example types are: random
  forests, logistic regression, linear support vector machines, etc.
- The **mode** of the model denotes how it will be used. Two common
  modes are *classification* and *regression*. Others would include
  “censored regression” and “risk regression” (parametric and Cox PH
  models for censored data, respectively), as well as unsupervised
  models (e.g. “clustering”).
- The **computational engine** indicates how the actual model might be
  fit. These are often R packages (such as `randomForest` or `ranger`)
  but might also be methods outside of R (e.g. Stan, Spark, and others).

The parsnip package, similar to ggplot2, dplyr and recipes, separates
the specification of what you want to do from the actual doing. This
allows us to create broader functionality for modeling.

### Placeholders for Parameters

There are times where you would like to change a parameter from its
default but you are not sure what the final value will be. This is the
basis for *model tuning* where we use the
[tune](https://tune.tidymodels.org/) package. Since the model is not
executing when created, these types of parameters can be changed using
the [`tune()`](https://hardhat.tidymodels.org/reference/tune.html)
function. This provides a simple placeholder for the value.

``` r
tune_mtry <- rand_forest(trees = 2000, mtry = tune())
tune_mtry
#> Random Forest Model Specification (unknown mode)
#> 
#> Main Arguments:
#>   mtry = tune()
#>   trees = 2000
#> 
#> Computational engine: ranger
```

This will come in handy later when we fit the model over different
values of `mtry`.

### Specifying Arguments

Commonly used arguments to the modeling functions have their parameters
exposed in the function. For example, `rand_forest` has arguments for:

- `mtry`: The number of predictors that will be randomly sampled at each
  split when creating the tree models.
- `trees`: The number of trees contained in the ensemble.
- `min_n`: The minimum number of data points in a node that are required
  for the node to be split further.

The arguments to the default function are:

``` r
args(rand_forest)
#> function (mode = "unknown", engine = "ranger", mtry = NULL, trees = NULL, 
#>     min_n = NULL) 
#> NULL
```

However, there might be other arguments that you would like to change or
allow to vary. These are accessible using `set_engine`. For example,
[`ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md) from
the ranger package has an option to set the internal random number seed.
To set this to a specific value:

``` r
rf_with_seed <- 
  rand_forest(trees = 2000, mtry = tune(), mode = "regression") |>
  set_engine("ranger", seed = 63233)
rf_with_seed
#> Random Forest Model Specification (regression)
#> 
#> Main Arguments:
#>   mtry = tune()
#>   trees = 2000
#> 
#> Engine-Specific Arguments:
#>   seed = 63233
#> 
#> Computational engine: ranger
```

### Process

To fit the model, you must:

- have a defined model, including the *mode*,
- have no [`tune()`](https://hardhat.tidymodels.org/reference/tune.html)
  parameters, and
- specify a computational engine.

For example, `rf_with_seed` above is not ready for fitting due the
[`tune()`](https://hardhat.tidymodels.org/reference/tune.html)
parameter. We can set that parameter’s value and then create the model
fit:

``` r
rf_with_seed |> 
  set_args(mtry = 4) |> 
  set_engine("ranger") |>
  fit(mpg ~ ., data = mtcars)
```

    #> parsnip model object
    #> 
    #> Ranger result
    #> 
    #> Call:
    #>  ranger::ranger(x = maybe_data_frame(x), y = y, mtry = min_cols(~4, x), num.trees = ~2000, num.threads = 1, verbose = FALSE, seed = sample.int(10^5, 1)) 
    #> 
    #> Type:                             Regression 
    #> Number of trees:                  2000 
    #> Sample size:                      32 
    #> Number of independent variables:  10 
    #> Mtry:                             4 
    #> Target node size:                 5 
    #> Variable importance mode:         none 
    #> Splitrule:                        variance 
    #> OOB prediction error (MSE):       5.57 
    #> R squared (OOB):                  0.847

Or, using the `randomForest` package:

``` r
set.seed(56982)
rf_with_seed |> 
  set_args(mtry = 4) |> 
  set_engine("randomForest") |>
  fit(mpg ~ ., data = mtcars)
```

    #> parsnip model object
    #> 
    #> 
    #> Call:
    #>  randomForest(x = maybe_data_frame(x), y = y, ntree = ~2000, mtry = min_cols(~4, x)) 
    #>                Type of random forest: regression
    #>                      Number of trees: 2000
    #> No. of variables tried at each split: 4
    #> 
    #>           Mean of squared residuals: 5.52
    #>                     % Var explained: 84.3

Note that the call objects show `num.trees = ~2000`. The tilde is the
consequence of `parsnip` using
[quosures](https://adv-r.hadley.nz/evaluation.html#quosures) to process
the model specification’s arguments.

Normally, when a function is executed, the function’s arguments are
immediately evaluated. In the case of parsnip, the model specification’s
arguments are *not*; the [expression is
captured](https://tidyverse.org/blog/2019/04/parsnip-internals/) along
with the environment where it should be evaluated. That is what a
quosure does.

parsnip uses these expressions to make a model fit call that is
evaluated. The tilde in the call above reflects that the argument was
captured using a quosure.
