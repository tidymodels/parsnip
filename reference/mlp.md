# Single layer neural network

`mlp()` defines a multilayer perceptron model (a.k.a. a single layer,
feed-forward neural network). This function can fit classification and
regression models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`nnet`](https://parsnip.tidymodels.org/reference/details_mlp_nnet.md)`¹`

- [`brulee`](https://parsnip.tidymodels.org/reference/details_mlp_brulee.md)

- [`brulee_two_layer`](https://parsnip.tidymodels.org/reference/details_mlp_brulee_two_layer.md)

- [`h2o`](https://parsnip.tidymodels.org/reference/details_mlp_h2o.md)`²`

- [`keras`](https://parsnip.tidymodels.org/reference/details_mlp_keras.md)

¹ The default engine. ² Requires a parsnip extension package for
classification and regression.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
mlp(
  mode = "unknown",
  engine = "nnet",
  hidden_units = NULL,
  penalty = NULL,
  dropout = NULL,
  epochs = NULL,
  activation = NULL,
  learn_rate = NULL
)
```

## Arguments

- mode:

  A single character string for the prediction outcome mode. Possible
  values for this model are "unknown", "regression", or
  "classification".

- engine:

  A single character string specifying what computational engine to use
  for fitting.

- hidden_units:

  An integer for the number of units in the hidden model.

- penalty:

  A non-negative numeric value for the amount of weight decay.

- dropout:

  A number between 0 (inclusive) and 1 denoting the proportion of model
  parameters randomly set to zero during model training.

- epochs:

  An integer for the number of training iterations.

- activation:

  A single character string denoting the type of relationship between
  the original predictors and the hidden unit layer. The activation
  function between the hidden and output layers is automatically set to
  either "linear" or "softmax" depending on the type of outcome.
  Possible values depend on the engine being used.

- learn_rate:

  A number for the rate at which the boosting algorithm adapts from
  iteration-to-iteration (specific engines only). This is sometimes
  referred to as the shrinkage parameter.

## Details

This function only defines what *type* of model is being fit. Once an
engine is specified, the *method* to fit the model is also defined. See
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md)
for more on setting the engine, including how to set engine arguments.

The model is not trained or fit until the
[`fit()`](https://parsnip.tidymodels.org/reference/fit.md) function is
used with the data.

Each of the arguments in this function other than `mode` and `engine`
are captured as
[quosures](https://rlang.r-lib.org/reference/topic-quosure.html). To
pass values programmatically, use the [injection
operator](https://rlang.r-lib.org/reference/injection-operator.html)
like so:

    value <- 1
    mlp(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

[`fit()`](https://parsnip.tidymodels.org/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`nnet engine details`](https://parsnip.tidymodels.org/reference/details_mlp_nnet.md),
[`brulee engine details`](https://parsnip.tidymodels.org/reference/details_mlp_brulee.md),
[`brulee_two_layer engine details`](https://parsnip.tidymodels.org/reference/details_mlp_brulee_two_layer.md),
[`h2o engine details`](https://parsnip.tidymodels.org/reference/details_mlp_h2o.md),
[`keras engine details`](https://parsnip.tidymodels.org/reference/details_mlp_keras.md)

## Examples

``` r
show_engines("mlp")
#> # A tibble: 8 × 2
#>   engine           mode          
#>   <chr>            <chr>         
#> 1 keras            classification
#> 2 keras            regression    
#> 3 nnet             classification
#> 4 nnet             regression    
#> 5 brulee           classification
#> 6 brulee           regression    
#> 7 brulee_two_layer classification
#> 8 brulee_two_layer regression    

mlp(mode = "classification", penalty = 0.01)
#> Single Layer Neural Network Model Specification (classification)
#> 
#> Main Arguments:
#>   penalty = 0.01
#> 
#> Computational engine: nnet 
#> 
```
