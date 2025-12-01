# Simple interface to MLP models via keras

Instead of building a `keras` model sequentially, `keras_mlp` can be
used to create a feedforward network with a single hidden layer.
Regularization is via either weight decay or dropout.

## Usage

``` r
keras_mlp(
  x,
  y,
  hidden_units = 5,
  penalty = 0,
  dropout = 0,
  epochs = 20,
  activation = "softmax",
  seeds = sample.int(10^5, size = 3),
  ...
)
```

## Arguments

- x:

  A data frame or matrix of predictors

- y:

  A vector (factor or numeric) or matrix (numeric) of outcome data.

- hidden_units:

  An integer for the number of hidden units.

- penalty:

  A non-negative real number for the amount of weight decay. Either this
  parameter *or* `dropout` can specified.

- dropout:

  The proportion of parameters to set to zero. Either this parameter
  *or* `penalty` can specified.

- epochs:

  An integer for the number of passes through the data.

- activation:

  A character string for the type of activation function between layers.

- seeds:

  A vector of three positive integers to control randomness of the
  calculations.

- ...:

  Additional named arguments to pass to
  [`keras::compile()`](https://generics.r-lib.org/reference/compile.html)
  or [`keras::fit()`](https://generics.r-lib.org/reference/fit.html).
  Arguments will be sorted and passed to either function internally.

## Value

A `keras` model object.
