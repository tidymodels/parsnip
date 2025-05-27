


# tidymodels and glmnet

The implementation of the glmnet package has some nice features. For example, one of the main tuning parameters, the regularization penalty,  does not need to be specified when fitting the model. The package fits a compendium of values, called the regularization path. These values depend on the data set and the value of `alpha`, the mixture parameter between a pure ridge model (`alpha = 0`) and a pure lasso model (`alpha = 1`). When predicting, any penalty values can be simultaneously predicted, even those that are not exactly on the regularization path. For those, the model approximates between the closest path values to produce a prediction. There is an argument called `lambda` to the `glmnet()` function that is used to specify the path. 

In the discussion below, `linear_reg()` is used. The information is true for all parsnip models that have a `"glmnet"` engine. 

## Fitting and predicting using parsnip

Recall that tidymodels uses standardized parameter names across models chosen to be low on jargon. The argument `penalty` is the equivalent of what glmnet calls the `lambda` value and `mixture` is the same as their `alpha` value. 

In tidymodels, our `predict()` methods are defined to make one prediction at a time. For this model, that means predictions are for a single penalty value. For this reason, models that have glmnet engines require the user to always specify a single penalty value when the model is defined. For example, for linear regression: 

```r
linear_reg(penalty = 1) |> set_engine("glmnet")
```

When the `predict()` method is called, it automatically uses the penalty that was given when the model was defined. For example: 



``` r
library(tidymodels)

fit <- 
  linear_reg(penalty = 1) |> 
  set_engine("glmnet") |> 
  fit(mpg ~ ., data = mtcars)

# predict at penalty = 1
predict(fit, mtcars[1:3,])
```

```
## # A tibble: 3 x 1
##   .pred
##   <dbl>
## 1  22.2
## 2  21.5
## 3  24.9
```

However, any penalty values can be predicted simultaneously using the `multi_predict()` method: 


``` r
# predict at c(0.00, 0.01)
multi_predict(fit, mtcars[1:3,], penalty = c(0.00, 0.01))
```

```
## # A tibble: 3 x 1
##   .pred           
##   <list>          
## 1 <tibble [2 x 2]>
## 2 <tibble [2 x 2]>
## 3 <tibble [2 x 2]>
```

``` r
# unnested:
multi_predict(fit, mtcars[1:3,], penalty = c(0.00, 0.01)) |> 
  add_rowindex() |> 
  unnest(cols = ".pred")
```

```
## # A tibble: 6 x 3
##   penalty .pred  .row
##     <dbl> <dbl> <int>
## 1    0     22.6     1
## 2    0.01  22.5     1
## 3    0     22.1     2
## 4    0.01  22.1     2
## 5    0     26.3     3
## 6    0.01  26.3     3
```

### Where did `lambda` go? 

It may appear odd that the `lambda` value does not get used in the fit: 


``` r
linear_reg(penalty = 1) |> 
  set_engine("glmnet") |> 
  translate()
```

```
## Linear Regression Model Specification (regression)
## 
## Main Arguments:
##   penalty = 1
## 
## Computational engine: glmnet 
## 
## Model fit template:
## glmnet::glmnet(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     family = "gaussian")
```

Internally, the value of `penalty = 1` is saved in the parsnip object and no value is set for `lambda`. This enables the full path to be fit by `glmnet()`. See the section below about setting the path. 

## How do I set the regularization path? 

Regardless of what value you use for `penalty`, the full coefficient path is used when [glmnet::glmnet()] is called. 

What if you want to manually set this path? Normally, you would pass a vector to `lambda` in [glmnet::glmnet()]. 

parsnip models that use a `glmnet` engine can use a special optional argument called `path_values`. This is _not_ an argument to [glmnet::glmnet()]; it is used by parsnip to independently set the path. 

For example, we have found that if you want a fully ridge regression model (i.e., `mixture = 0`), you can get the _wrong coefficients_ if the path does not contain zero (see [issue #431](https://github.com/tidymodels/parsnip/issues/431#issuecomment-782883848)). 

If we want to use our own path, the argument is passed as an engine-specific option:


``` r
coef_path_values <- c(0, 10^seq(-5, 1, length.out = 7))

fit_ridge <- 
  linear_reg(penalty = 1, mixture = 0) |> 
  set_engine("glmnet", path_values = coef_path_values) |> 
  fit(mpg ~ ., data = mtcars)

all.equal(sort(fit_ridge$fit$lambda), coef_path_values)
```

```
## [1] TRUE
```

``` r
# predict at penalty = 1
predict(fit_ridge, mtcars[1:3,])
```

```
## # A tibble: 3 x 1
##   .pred
##   <dbl>
## 1  22.1
## 2  21.8
## 3  26.6
```

## Tidying the model object

[broom::tidy()] is a function that gives a summary of the object as a tibble. 

**tl;dr** `tidy()` on a `glmnet` model produced by parsnip gives the coefficients for the value given by `penalty`. 

When parsnip makes a model, it gives it an extra class. Use the `tidy()` method on the object, it produces coefficients for the penalty that was originally requested: 


``` r
tidy(fit)
```

```
## # A tibble: 11 x 3
##   term        estimate penalty
##   <chr>          <dbl>   <dbl>
## 1 (Intercept)  35.3          1
## 2 cyl          -0.872        1
## 3 disp          0            1
## 4 hp           -0.0101       1
## 5 drat          0            1
## 6 wt           -2.59         1
## # i 5 more rows
```

Note that there is a `tidy()` method for `glmnet` objects in the `broom` package. If this is used directly on the underlying `glmnet` object, it returns _all of coefficients on the path_:


``` r
# Use the basic tidy() method for glmnet
all_tidy_coefs <- broom:::tidy.glmnet(fit$fit)
all_tidy_coefs
```

```
## # A tibble: 640 x 5
##   term         step estimate lambda dev.ratio
##   <chr>       <dbl>    <dbl>  <dbl>     <dbl>
## 1 (Intercept)     1     20.1   5.15     0    
## 2 (Intercept)     2     21.6   4.69     0.129
## 3 (Intercept)     3     23.2   4.27     0.248
## 4 (Intercept)     4     24.7   3.89     0.347
## 5 (Intercept)     5     26.0   3.55     0.429
## 6 (Intercept)     6     27.2   3.23     0.497
## # i 634 more rows
```

``` r
length(unique(all_tidy_coefs$lambda))
```

```
## [1] 79
```

This can be nice for plots but it might not contain the penalty value that you are interested in. 
