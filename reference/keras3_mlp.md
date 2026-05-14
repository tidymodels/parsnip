# Simple interface to MLP models via keras3

Instead of building a `keras3` model sequentially, `keras3_mlp` can be
used to create a feedforward network with a single hidden layer.
Regularization is via either weight decay or dropout.

## Usage

``` r
keras3_mlp(
  x,
  y,
  hidden_units = 5,
  penalty = 0,
  dropout = 0,
  epochs = 20,
  activation = "softmax",
  seed = sample.int(10^5, size = 1),
  ...
)
```

## Arguments

- x:

  A data frame or matrix of predictors.

- y:

  A vector (factor or numeric) or matrix (numeric) of outcome data.

- hidden_units:

  An integer for the number of hidden units.

- penalty:

  A non-negative real number for the amount of weight decay. Either this
  parameter *or* `dropout` can be specified.

- dropout:

  The proportion of parameters to set to zero. Either this parameter
  *or* `penalty` can be specified.

- epochs:

  An integer for the number of passes through the data.

- activation:

  A character string for the type of activation function between layers.

- seed:

  A single positive integer to control randomness.

- ...:

  Additional named arguments to pass to
  [`keras3::compile()`](https://generics.r-lib.org/reference/compile.html)
  or [`keras3::fit()`](https://generics.r-lib.org/reference/fit.html).

## Value

A `keras3` model object.
