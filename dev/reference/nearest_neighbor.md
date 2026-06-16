# K-nearest neighbors

`nearest_neighbor()` defines a model that uses the `K` most similar data
points from the training set to predict new samples. This function can
fit classification and regression models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`kknn`](https://parsnip.tidymodels.org/dev/reference/details_nearest_neighbor_kknn.md)`¹`

¹ The default engine.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
nearest_neighbor(
  mode = "unknown",
  engine = "kknn",
  neighbors = NULL,
  weight_func = NULL,
  dist_power = NULL
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

- neighbors:

  A single integer for the number of neighbors to consider (often called
  `k`). For kknn, a value of 5 is used if `neighbors` is not specified.

- weight_func:

  A *single* character for the type of kernel function used to weight
  distances between samples. Valid choices are: `"rectangular"`,
  `"triangular"`, `"epanechnikov"`, `"biweight"`, `"triweight"`,
  `"cos"`, `"inv"`, `"gaussian"`, `"rank"`, or `"optimal"`.

- dist_power:

  A single number for the parameter used in calculating Minkowski
  distance.

## Details

This function only defines what *type* of model is being fit. Once an
engine is specified, the *method* to fit the model is also defined. See
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md)
for more on setting the engine, including how to set engine arguments.

The model is not trained or fit until the
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md) function
is used with the data.

Each of the arguments in this function other than `mode` and `engine`
are captured as
[quosures](https://rlang.r-lib.org/reference/topic-quosure.html). To
pass values programmatically, use the [injection
operator](https://rlang.r-lib.org/reference/injection-operator.html)
like so:

    value <- 1
    nearest_neighbor(argument = !!value)

## References

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`kknn engine details`](https://parsnip.tidymodels.org/dev/reference/details_nearest_neighbor_kknn.md)

## Examples

``` r
show_engines("nearest_neighbor")
#> # A tibble: 2 × 2
#>   engine mode          
#>   <chr>  <chr>         
#> 1 kknn   classification
#> 2 kknn   regression    

nearest_neighbor(neighbors = 11)
#> K-Nearest Neighbor Model Specification (unknown mode)
#> 
#> Main Arguments:
#>   neighbors = 11
#> 
#> Computational engine: kknn 
#> 
```
