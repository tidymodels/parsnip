# Boosted trees via lightgbm

`lightgbm::lgb.train()` creates a series of decision trees forming an
ensemble. Each tree depends on the results of previous trees. All trees
in the ensemble are combined to produce a final prediction.

## Details

For this engine, there are multiple modes: regression and classification

### Tuning Parameters

This model has 6 tuning parameters:

- `tree_depth`: Tree Depth (type: integer, default: -1)

- `trees`: \# Trees (type: integer, default: 100)

- `learn_rate`: Learning Rate (type: double, default: 0.1)

- `mtry`: \# Randomly Selected Predictors (type: integer, default: see
  below)

- `min_n`: Minimal Node Size (type: integer, default: 20)

- `loss_reduction`: Minimum Loss Reduction (type: double, default: 0)

The `mtry` parameter gives the *number* of predictors that will be
randomly sampled at each split. The default is to use all predictors.

Rather than as a number, `lightgbm::lgb.train()`’s `feature_fraction`
argument encodes `mtry` as the *proportion* of predictors that will be
randomly sampled at each split. parsnip translates `mtry`, supplied as
the *number* of predictors, to a proportion under the hood. That is, the
user should still supply the argument as `mtry` to
[`boost_tree()`](https://parsnip.tidymodels.org/dev/reference/boost_tree.md),
and do so in its sense as a number rather than a proportion; before
passing `mtry` to `lightgbm::lgb.train()`, parsnip will convert the
`mtry` value to a proportion.

Note that parsnip’s translation can be overridden via the `counts`
argument, supplied to
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md).
By default, `counts` is set to `TRUE`, but supplying the argument
`counts = FALSE` allows the user to supply `mtry` as a proportion rather
than a number.

### Translation from parsnip to the original package (regression)

The **bonsai** extension package is required to fit this model.

    boost_tree(
      mtry = integer(), trees = integer(), tree_depth = integer(),
      learn_rate = numeric(), min_n = integer(), loss_reduction = numeric()
    ) |>
      set_engine("lightgbm") |>
      set_mode("regression") |>
      translate()

    ## Boosted Tree Model Specification (regression)
    ##
    ## Main Arguments:
    ##   mtry = integer()
    ##   trees = integer()
    ##   min_n = integer()
    ##   tree_depth = integer()
    ##   learn_rate = numeric()
    ##   loss_reduction = numeric()
    ##
    ## Computational engine: lightgbm
    ##
    ## Model fit template:
    ## bonsai::train_lightgbm(x = missing_arg(), y = missing_arg(),
    ##     weights = missing_arg(), feature_fraction_bynode = integer(),
    ##     num_iterations = integer(), min_data_in_leaf = integer(),
    ##     max_depth = integer(), learning_rate = numeric(), min_gain_to_split = numeric(),
    ##     verbose = -1, num_threads = 0, seed = sample.int(10^5, 1),
    ##     deterministic = TRUE)

### Translation from parsnip to the original package (classification)

The **bonsai** extension package is required to fit this model.

    boost_tree(
      mtry = integer(), trees = integer(), tree_depth = integer(),
      learn_rate = numeric(), min_n = integer(), loss_reduction = numeric()
    ) |>
      set_engine("lightgbm") |>
      set_mode("classification") |>
      translate()

    ## Boosted Tree Model Specification (classification)
    ##
    ## Main Arguments:
    ##   mtry = integer()
    ##   trees = integer()
    ##   min_n = integer()
    ##   tree_depth = integer()
    ##   learn_rate = numeric()
    ##   loss_reduction = numeric()
    ##
    ## Computational engine: lightgbm
    ##
    ## Model fit template:
    ## bonsai::train_lightgbm(x = missing_arg(), y = missing_arg(),
    ##     weights = missing_arg(), feature_fraction_bynode = integer(),
    ##     num_iterations = integer(), min_data_in_leaf = integer(),
    ##     max_depth = integer(), learning_rate = numeric(), min_gain_to_split = numeric(),
    ##     verbose = -1, num_threads = 0, seed = sample.int(10^5, 1),
    ##     deterministic = TRUE)

`bonsai::train_lightgbm()` is a wrapper around `lightgbm::lgb.train()`
(and other functions) that make it easier to run this model.

### Other details

#### Preprocessing

This engine does not require any special encoding of the predictors.
Categorical predictors can be partitioned into groups of factor levels
(e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables
are not required for this model.

Non-numeric predictors (i.e., factors) are internally converted to
numeric. In the classification context, non-numeric outcomes (i.e.,
factors) are also internally converted to numeric.

### Case weights

This model can utilize case weights during model fitting. To use them,
see the documentation in
[case_weights](https://parsnip.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

The [`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) arguments
have arguments called `case_weights` that expect vectors of case
weights.

Although the source documentation is unclear about how the weights are
utilized, it appears that the weights are applied to the objective
function, not just the sampling mechanism. A GitHub issue
(<https://github.com/microsoft/LightGBM/issues/1299>) has evidence that
the weights are a “multiplication applied to every positive label
weight” and shows some C++ code to that effect.

#### Interpreting `mtry`

The `mtry` argument denotes the number of predictors that will be
randomly sampled at each split when creating tree models.

Some engines, such as `"xgboost"`, `"xrf"`, and `"lightgbm"`, interpret
their analogue to the `mtry` argument as the *proportion* of predictors
that will be randomly sampled at each split rather than the *count*. In
some settings, such as when tuning over preprocessors that influence the
number of predictors, this parameterization is quite
helpful—interpreting `mtry` as a proportion means that `[0, 1]` is
always a valid range for that parameter, regardless of input data.

parsnip and its extensions accommodate this parameterization using the
`counts` argument: a logical indicating whether `mtry` should be
interpreted as the number of predictors that will be randomly sampled at
each split. `TRUE` indicates that `mtry` will be interpreted in its
sense as a count, `FALSE` indicates that the argument will be
interpreted in its sense as a proportion.

`mtry` is a main model argument for
[`boost_tree()`](https://parsnip.tidymodels.org/dev/reference/boost_tree.md)
and
[`rand_forest()`](https://parsnip.tidymodels.org/dev/reference/rand_forest.md),
and thus should not have an engine-specific interface. So, regardless of
engine, `counts` defaults to `TRUE`. For engines that support the
proportion interpretation (currently `"xgboost"` and `"xrf"`, via the
rules package, and `"lightgbm"` via the bonsai package) the user can
pass the `counts = FALSE` argument to
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md)
to supply `mtry` values within `[0, 1]`.

### Prediction types

    parsnip:::get_from_env("boost_tree_predict") |>
      dplyr::filter(engine == "lightgbm") |>
      dplyr::select(mode, type)

    ## # A tibble: 4 x 2
    ##   mode           type
    ##   <chr>          <chr>
    ## 1 regression     numeric
    ## 2 classification class
    ## 3 classification prob
    ## 4 classification raw

#### Bagging

The `sample_size` argument is translated to the `bagging_fraction`
parameter in the `param` argument of `lgb.train`. The argument is
interpreted by lightgbm as a *proportion* rather than a count, so bonsai
internally reparameterizes the `sample_size` argument with
[`dials::sample_prop()`](https://dials.tidymodels.org/reference/trees.html)
during tuning.

To effectively enable bagging, the user would also need to set the
`bagging_freq` argument to lightgbm. `bagging_freq` defaults to 0, which
means bagging is disabled, and a `bagging_freq` argument of `k` means
that the booster will perform bagging at every `k`th boosting iteration.
Thus, by default, the `sample_size` argument would be ignored without
setting this argument manually. Other boosting libraries, like xgboost,
do not have an analogous argument to `bagging_freq` and use `k = 1` when
the analogue to `bagging_fraction` is in \$`(0, 1)`\$. *bonsai will thus
automatically set* `bagging_freq = 1` *in* `set_engine("lightgbm", ...)`
if `sample_size` (i.e. `bagging_fraction`) is not equal to 1 and no
`bagging_freq` value is supplied. This default can be overridden by
setting the `bagging_freq` argument to
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md)
manually.

#### Verbosity

bonsai quiets much of the logging output from `lightgbm::lgb.train()` by
default. With default settings, logged warnings and errors will still be
passed on to the user. To print out all logs during training, set
`quiet = TRUE`.

### Sparse Data

This model can utilize sparse data during model fitting and prediction.
Both sparse matrices such as dgCMatrix from the `Matrix` package and
sparse tibbles from the `sparsevctrs` package are supported. See
[sparse_data](https://parsnip.tidymodels.org/dev/reference/sparse_data.md)
for more information.

### Examples

The “Introduction to bonsai” article contains
[examples](https://bonsai.tidymodels.org/articles/bonsai.html) of
[`boost_tree()`](https://parsnip.tidymodels.org/dev/reference/boost_tree.md)
with the `"lightgbm"` engine.

### References

- [LightGBM: A Highly Efficient Gradient Boosting Decision
  Tree](https://papers.nips.cc/paper/2017/hash/6449f44a102fde848669bdd9eb6b76fa-Abstract.html)

- Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.
