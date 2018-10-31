
[![Travis build status](https://travis-ci.org/tidymodels/parsnip.svg?branch=master)](https://travis-ci.org/tidymodels/parsnip)
[![Coverage status](https://codecov.io/gh/tidymodels/parsnip/branch/master/graph/badge.svg)](https://codecov.io/github/tidymodels/parsnip?branch=master)
![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)


One issue with different functions available in R _that do the same thing_ is that they can have different interfaces and arguments. For example, to fit a random forest _classification_ model, we might have:

```r
# From randomForest
rf_1 <- randomForest(x, y, mtry = 12, ntree = 2000, importance = TRUE)

# From ranger
rf_2 <- ranger(
  y ~ ., 
  data = dat, 
  mtry = 12, 
  num.trees = 2000, 
  importance = 'impurity'
)

# From sparklyr
rf_3 <- ml_random_forest(
  dat, 
  intercept = FALSE, 
  response = "y", 
  features = names(dat)[names(dat) != "y"], 
  col.sample.rate = 12,
  num.trees = 2000
)
```

Note that the model syntax is very different and that the argument names (and formats) are also different. This is a pain if you go between implementations. 

In this example, 

 * the **type** of model is "random forest" 
 * the **mode** of the model is "classification" (as opposed to regression,  etc). 
 * the computational **engine** is the name of the R package. 


The idea of `parsnip` is to:

* Separate the definition of a model from its evaluation.
* Decouple the model specification from the implementation (whether the implementation is in R, spark, or something else). For example, the user would call `rand_forest` instead of `ranger::ranger` or other specific packages. 
* Harmonize the argument names (e.g. `n.trees`, `ntrees`, `trees`) so that users can remember a single name. This will help _across_ model types too so that `trees` will be the same argument across random forest as well as boosting or bagging. 

Using the example above, the `parsnip` approach would be

```r
rand_forest(mtry = 12, trees = 2000) %>%
  set_engine("ranger", importance = 'impurity') %>%
  fit(y ~ ., data = dat)
```

The engine can be easily changed and the mode can be determined when `fit` is called. To use Spark, the change is simple:

```r
rand_forest(mtry = 12, trees = 2000) %>%
  set_engine("spark") %>%
  fit(y ~ ., data = dat)
```



To install it, use:

```r
require(devtools)
install_github("tidymodels/parsnip")
```
