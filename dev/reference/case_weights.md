# Using case weights with parsnip

Case weights are positive numeric values that influence how much each
data point has during the model fitting process. There are a variety of
situations where case weights can be used.

## Details

tidymodels packages differentiate *how* different types of case weights
should be used during the entire data analysis process, including
preprocessing data, model fitting, performance calculations, etc.

The tidymodels packages require users to convert their numeric vectors
to a vector class that reflects how these should be used. For example,
there are some situations where the weights should not affect operations
such as centering and scaling or other preprocessing operations.

The types of weights allowed in tidymodels are:

- Frequency weights via
  [`hardhat::frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html)

- Importance weights via
  [`hardhat::importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html)

More types can be added by request.

For parsnip, the
[`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) functions
contain a `case_weight` argument that takes these data. For Spark
models, the argument value should be a character value.

## See also

[`frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html),
[`importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html),
[`fit()`](https://generics.r-lib.org/reference/fit.html),
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html)
