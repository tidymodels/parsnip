# Set seed in R and TensorFlow at the same time

Some Keras models requires seeds to be set in both R and TensorFlow to
achieve reproducible results. This function sets these seeds at the same
time using version appropriate functions.

## Usage

``` r
set_tf_seed(seed)
```

## Arguments

- seed:

  1 integer value.
