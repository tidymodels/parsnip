One issues with different functions available in R _to do the same thing_ is that they can have different interfaces and arguments. For example, to fit a random forest _classification_ model, we might have:

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

In this example, the **type** of model is "random forest" while the **mode** of the model is "classification" (as opposed to regression, survival analysis, etc). 


The idea of `parsnip` is to:

* Separate the definition of a model from its evaluation.
* Decouple the model specification from the implementation (whether the implementation is in R, spark, or something else). For example, the user would call `rand_forest` instead of `ranger::ranger` or other specific packages. 
* Harmonize the argument names (e.g. `n.trees`, `ntrees`, `trees`) so that users can remember a single name. This will help _across_ model types too so that `trees` will be the same argument across random forest as well as boosting or bagging. 


The process:

1). user invokes a wrapper function to specify the model type, for example:

```r
mod  <- rand_forest(mpg ~ ., data = mtcars, trees = 200, mtry = varying())
```

Notes

* This includes some arguments that are placeholders for a value to be named later (e.g. `varying()`)
* `trees` is not an argument to the supported functions and will be translated to the appropriate argument.
* Other interfaces, such as recipes, will be available

2). Based on the type of data in the outcome, the object `mod` is given a class that reflects its `mode`, such as `rand_forest.regression`. 

3). The extra arguments are saved in a list without being evaluated. 

4) An "engine" is declared for what software will be used. In this example, it could be "R:ranger", or "R:randomForests", or "Spark:RandomForestModel".

5). Given the engine, a default expression is used to: 

 * Convert a generic argument name into one specific to the engine (`n_trees` is replaced by `num.trees` for `ranger`)
 * The value for the argument is substituted into the expression.
 * Any arguments that would be passed to `...` are also added. 

6). A few other pieces of information are recorded based on the engine:

 * The interface of the fitting expression (e.g. formula, x/y, recipes).
 * A list of packages required to fit the model.
 * Any functions required to be run on the data prior to evaluating the model. 
 * A flag to indicate whether all the arguments can be evaluated. In the example above, `varying()` would set this to `FALSE` but an expression such as `floor(ncol(x)/3)` would not. 

7). The `fit` class can be used to evaluate the expression on the data being used to estimate the parameters. The fitted model is returned. 

 