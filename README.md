The idea of `parsnip` is to:

* Allow users to specify a model before it is evaluated.
* Decouple the model specification from the implementation (whether the implementation is in R, spark, or something else). For example, the user would call `rand_forest` instead of `ranger::ranger` or other specific packages. 
* Harmonize the argument names (e.g. `n.trees`, `ntrees`, `trees`) so that users can remember a single name.


The process:

1). user invokes a wrapper function to specify the model type, for example:

```r
mod  <- rand_forest(mpg ~ ., data = mtcars, n_trees = 200, mtry = param("mtry"))
```

Notes

* This includes some arguments that are placeholders for a value to be named later (e.g. `param("mtry")`)
* `n_trees` is not an argument to the supported functions and will be translated to the appropriate argument.
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
 * A flag to indicate whether all the arguments can be evaluated. In the example above, `param("mtry")` would set this to `FALSE` but an expression such as `floor(ncol(x)/3)` would not. 

7). The `fit` class can be used to evaluate the expression on the data being used to estimate the parameters. The fitted model is returned. 

 