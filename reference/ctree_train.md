# A wrapper function for conditional inference tree models

These functions are slightly different APIs for
[`partykit::ctree()`](https://rdrr.io/pkg/partykit/man/ctree.html) and
[`partykit::cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html)
that have several important arguments as top-level arguments (as opposed
to being specified in
[`partykit::ctree_control()`](https://rdrr.io/pkg/partykit/man/ctree_control.html)).

## Usage

``` r
ctree_train(
  formula,
  data,
  weights = NULL,
  minsplit = 20L,
  maxdepth = Inf,
  teststat = "quadratic",
  testtype = "Bonferroni",
  mincriterion = 0.95,
  ...
)

cforest_train(
  formula,
  data,
  weights = NULL,
  minsplit = 20L,
  maxdepth = Inf,
  teststat = "quadratic",
  testtype = "Univariate",
  mincriterion = 0,
  mtry = ceiling(sqrt(ncol(data) - 1)),
  ntree = 500L,
  ...
)
```

## Arguments

- formula:

  A symbolic description of the model to be fit.

- data:

  A data frame containing the variables in the model.

- weights:

  A vector of weights whose length is the same as `nrow(data)`. For
  [`partykit::ctree()`](https://rdrr.io/pkg/partykit/man/ctree.html)
  models, these are required to be non-negative integers while for
  [`partykit::cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html)
  they can be non-negative integers or doubles.

- minsplit:

  The minimum sum of weights in a node in order to be considered for
  splitting.

- maxdepth:

  maximum depth of the tree. The default `maxdepth = Inf` means that no
  restrictions are applied to tree sizes.

- teststat:

  A character specifying the type of the test statistic to be applied.

- testtype:

  A character specifying how to compute the distribution of the test
  statistic.

- mincriterion:

  The value of the test statistic (for `testtype == "Teststatistic"`),
  or 1 - p-value (for other values of `testtype`) that must be exceeded
  in order to implement a split.

- ...:

  Other options to pass to
  [`partykit::ctree()`](https://rdrr.io/pkg/partykit/man/ctree.html) or
  [`partykit::cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html).

- mtry:

  Number of input variables randomly sampled as candidates at each node
  for random forest like algorithms. The default `mtry = Inf` means that
  no random selection takes place.

- ntree:

  Number of trees to grow in a forest.

## Value

An object of class `party` (for `ctree`) or `cforest`.

## Examples

``` r
if (rlang::is_installed(c("modeldata", "partykit"))) {
  data(bivariate, package = "modeldata")
  ctree_train(Class ~ ., data = bivariate_train)
  ctree_train(Class ~ ., data = bivariate_train, maxdepth = 1)
}
#> 
#> Model formula:
#> Class ~ A + B
#> 
#> Fitted party:
#> [1] root
#> |   [2] B <= 56.77622: Two (n = 100, err = 34.0%)
#> |   [3] B > 56.77622: One (n = 909, err = 33.8%)
#> 
#> Number of inner nodes:    1
#> Number of terminal nodes: 2
```
