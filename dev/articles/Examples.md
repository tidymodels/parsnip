# Fitting and predicting with parsnip

These examples show how to *fit* and *predict* with different
combinations of model, mode, and engine. As a reminder, in parsnip,

- the **model type** differentiates basic modeling approaches, such as
  random forests, logistic regression, linear support vector machines,
  etc.,

- the **mode** denotes in what kind of modeling context it will be used
  (most commonly, classification or regression), and

- the computational **engine** indicates how the model is fit, such as
  with a specific R package implementation or even methods outside of R
  like Keras or Stan.

The following examples use consistent data sets throughout. For
regression, we use the Chicago ridership data. For classification, we
use an artificial data set for a binary example and the Palmer penguins
data for a multiclass example.

## `bart()` models

With the `"dbarts"` engine

### Regression Example (`dbarts`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
```

      ## ── Attaching packages ──────────────────────── tidymodels 1.4.1.9000 ──

      ## ✔ broom        1.0.11         ✔ rsample      1.3.1     
      ## ✔ dials        1.4.2          ✔ tailor       0.1.0     
      ## ✔ dplyr        1.1.4          ✔ tidyr        1.3.2     
      ## ✔ infer        1.1.0          ✔ tune         2.0.1     
      ## ✔ modeldata    1.5.1          ✔ workflows    1.3.0     
      ## ✔ parsnip      1.4.1.9000     ✔ workflowsets 1.1.1     
      ## ✔ purrr        1.2.1          ✔ yardstick    1.3.2     
      ## ✔ recipes      1.3.1

      ## ── Conflicts ──────────────────────────────── tidymodels_conflicts() ──
      ## ✖ purrr::discard() masks scales::discard()
      ## ✖ dplyr::filter()  masks stats::filter()
      ## ✖ dplyr::lag()     masks stats::lag()
      ## ✖ recipes::step()  masks stats::step()

``` r
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  bt_reg_spec <- 
    bart(trees = 15) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("regression") |> 
    set_engine("dbarts")
  bt_reg_spec
```

      ## BART Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   trees = 15
      ## 
      ## Computational engine: dbarts

Now we create the model fit object:

``` r
  set.seed(1)
  bt_reg_fit <- bt_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  bt_reg_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Call:
      ## `NULL`()

The holdout data can be predicted:

``` r
  predict(bt_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 20.1 
      ## 2 20.3 
      ## 3 21.3 
      ## 4 20.2 
      ## 5 19.4 
      ## 6  7.51
      ## 7  6.44

### Classification Example (`dbarts`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  bt_cls_spec <- 
    bart(trees = 15) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("classification") |> 
    set_engine("dbarts")
  bt_cls_spec
```

      ## 
      ## Call:
      ## NULL

Now we create the model fit object:

``` r
  set.seed(1)
  bt_cls_fit <- bt_cls_spec |> fit(Class ~ ., data = data_train)
  bt_cls_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Call:
      ## `NULL`()

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(bt_cls_fit, data_test),
    predict(bt_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class2             0.352        0.648
      ##  2 Class1             0.823        0.177
      ##  3 Class1             0.497        0.503
      ##  4 Class2             0.509        0.491
      ##  5 Class2             0.434        0.566
      ##  6 Class2             0.185        0.815
      ##  7 Class1             0.663        0.337
      ##  8 Class2             0.392        0.608
      ##  9 Class1             0.967        0.033
      ## 10 Class2             0.095        0.905

## `boost_tree()` models

With the `"xgboost"` engine

### Regression Example (`xgboost`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  bt_reg_spec <- 
    boost_tree(trees = 15) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("regression") |> 
    set_engine("xgboost")
  bt_reg_spec
```

      ## Boosted Tree Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   trees = 15
      ## 
      ## Computational engine: xgboost

Now we create the model fit object:

``` r
  set.seed(1)
  bt_reg_fit <- bt_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  bt_reg_fit
```

      ## parsnip model object
      ## 
      ## ##### xgb.Booster
      ## call:
      ##   xgboost::xgb.train(params = list(eta = 0.3, max_depth = 6, gamma = 0, 
      ##     colsample_bytree = 1, colsample_bynode = 1, min_child_weight = 1, 
      ##     subsample = 1, nthread = 1, objective = "reg:squarederror"), 
      ##     data = x$data, nrounds = 15, evals = x$watchlist, verbose = 0)
      ## # of features: 2 
      ## # of rounds:  15 
      ## callbacks:
      ##    evaluation_log 
      ## evaluation_log:
      ##   iter training_rmse
      ##  <num>         <num>
      ##      1      5.039503
      ##      2      4.080534
      ##    ---           ---
      ##     14      2.670995
      ##     15      2.658350

The holdout data can be predicted:

``` r
  predict(bt_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 20.5 
      ## 2 20.7 
      ## 3 20.7 
      ## 4 20.7 
      ## 5 19.4 
      ## 6  7.28
      ## 7  6.63

### Classification Example (`xgboost`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  bt_cls_spec <- 
    boost_tree(trees = 15) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("classification") |> 
    set_engine("xgboost")
  bt_cls_spec
```

      ## Boosted Tree Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   trees = 15
      ## 
      ## Computational engine: xgboost

Now we create the model fit object:

``` r
  set.seed(1)
  bt_cls_fit <- bt_cls_spec |> fit(Class ~ ., data = data_train)
  bt_cls_fit
```

      ## parsnip model object
      ## 
      ## ##### xgb.Booster
      ## call:
      ##   xgboost::xgb.train(params = list(eta = 0.3, max_depth = 6, gamma = 0, 
      ##     colsample_bytree = 1, colsample_bynode = 1, min_child_weight = 1, 
      ##     subsample = 1, nthread = 1, objective = "binary:logistic"), 
      ##     data = x$data, nrounds = 15, evals = x$watchlist, verbose = 0)
      ## # of features: 2 
      ## # of rounds:  15 
      ## callbacks:
      ##    evaluation_log 
      ## evaluation_log:
      ##   iter training_logloss
      ##  <num>            <num>
      ##      1        0.5506979
      ##      2        0.4714366
      ##    ---              ---
      ##     14        0.2615441
      ##     15        0.2579050

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(bt_cls_fit, data_test),
    predict(bt_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class2            0.197        0.803 
      ##  2 Class1            0.942        0.0583
      ##  3 Class1            0.602        0.398 
      ##  4 Class1            0.736        0.264 
      ##  5 Class2            0.170        0.830 
      ##  6 Class2            0.0848       0.915 
      ##  7 Class1            0.802        0.198 
      ##  8 Class1            0.641        0.359 
      ##  9 Class1            0.906        0.0937
      ## 10 Class2            0.0399       0.960

With the `"C5.0"` engine

### Classification Example (`C5.0`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  bt_cls_spec <- 
    boost_tree(trees = 15) |> 
    set_mode("classification") |> 
    set_engine("C5.0")
  bt_cls_spec
```

      ## Boosted Tree Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   trees = 15
      ## 
      ## Computational engine: C5.0

Now we create the model fit object:

``` r
  set.seed(1)
  bt_cls_fit <- bt_cls_spec |> fit(Class ~ ., data = data_train)
  bt_cls_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Call:
      ## C5.0.default(x = x, y = y, trials = 15, control
      ##  = C50::C5.0Control(minCases = 2, sample = 0))
      ## 
      ## Classification Tree
      ## Number of samples: 781 
      ## Number of predictors: 2 
      ## 
      ## Number of boosting iterations: 15 requested;  6 used due to early stopping
      ## Average tree size: 3.2 
      ## 
      ## Non-standard options: attempt to group attributes

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(bt_cls_fit, data_test),
    predict(bt_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class2             0.311        0.689
      ##  2 Class1             0.863        0.137
      ##  3 Class1             0.535        0.465
      ##  4 Class2             0.336        0.664
      ##  5 Class2             0.336        0.664
      ##  6 Class2             0.137        0.863
      ##  7 Class2             0.496        0.504
      ##  8 Class2             0.311        0.689
      ##  9 Class1             1            0    
      ## 10 Class2             0            1

## `decision_tree()` models

With the `"rpart"` engine

### Regression Example (`rpart`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  dt_reg_spec <- 
    decision_tree(tree_depth = 30) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("regression") |> 
    set_engine("rpart")
  dt_reg_spec
```

      ## Decision Tree Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   tree_depth = 30
      ## 
      ## Computational engine: rpart

Now we create the model fit object:

``` r
  set.seed(1)
  dt_reg_fit <- dt_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  dt_reg_fit
```

      ## parsnip model object
      ## 
      ## n= 5691 
      ## 
      ## node), split, n, deviance, yval
      ##       * denotes terminal node
      ## 
      ## 1) root 5691 244958.800 13.615560  
      ##   2) Quincy_Wells< 2.737 1721  22973.630  5.194394  
      ##     4) Clark_Lake< 5.07 1116  13166.830  4.260215 *
      ##     5) Clark_Lake>=5.07 605   7036.349  6.917607 *
      ##   3) Quincy_Wells>=2.737 3970  47031.540 17.266140  
      ##     6) Clark_Lake< 17.6965 1940  16042.090 15.418210 *
      ##     7) Clark_Lake>=17.6965 2030  18033.560 19.032140 *

The holdout data can be predicted:

``` r
  predict(dt_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 19.0 
      ## 2 19.0 
      ## 3 19.0 
      ## 4 19.0 
      ## 5 19.0 
      ## 6  6.92
      ## 7  6.92

### Classification Example (`rpart`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  dt_cls_spec <- 
    decision_tree(tree_depth = 30) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("classification") |> 
    set_engine("rpart")
  dt_cls_spec
```

      ## Decision Tree Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   tree_depth = 30
      ## 
      ## Computational engine: rpart

Now we create the model fit object:

``` r
  set.seed(1)
  dt_cls_fit <- dt_cls_spec |> fit(Class ~ ., data = data_train)
  dt_cls_fit
```

      ## parsnip model object
      ## 
      ## n= 781 
      ## 
      ## node), split, n, loss, yval, (yprob)
      ##       * denotes terminal node
      ## 
      ##  1) root 781 348 Class1 (0.5544174 0.4455826)  
      ##    2) B< 1.495535 400  61 Class1 (0.8475000 0.1525000) *
      ##    3) B>=1.495535 381  94 Class2 (0.2467192 0.7532808)  
      ##      6) B< 2.079458 191  70 Class2 (0.3664921 0.6335079)  
      ##       12) A>=2.572663 48  13 Class1 (0.7291667 0.2708333) *
      ##       13) A< 2.572663 143  35 Class2 (0.2447552 0.7552448) *
      ##      7) B>=2.079458 190  24 Class2 (0.1263158 0.8736842) *

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(dt_cls_fit, data_test),
    predict(dt_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class2             0.245        0.755
      ##  2 Class1             0.848        0.152
      ##  3 Class1             0.848        0.152
      ##  4 Class1             0.729        0.271
      ##  5 Class1             0.729        0.271
      ##  6 Class2             0.126        0.874
      ##  7 Class2             0.245        0.755
      ##  8 Class2             0.245        0.755
      ##  9 Class1             0.848        0.152
      ## 10 Class2             0.126        0.874

With the `"C5.0"` engine

### Classification Example (`C5.0`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  dt_cls_spec <- 
    decision_tree(min_n = 2) |> 
    set_mode("classification") |> 
    set_engine("C5.0")
  dt_cls_spec
```

      ## Decision Tree Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   min_n = 2
      ## 
      ## Computational engine: C5.0

Now we create the model fit object:

``` r
  set.seed(1)
  dt_cls_fit <- dt_cls_spec |> fit(Class ~ ., data = data_train)
  dt_cls_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Call:
      ## C5.0.default(x = x, y = y, trials = 1, control
      ##  = C50::C5.0Control(minCases = 2, sample = 0))
      ## 
      ## Classification Tree
      ## Number of samples: 781 
      ## Number of predictors: 2 
      ## 
      ## Tree size: 4 
      ## 
      ## Non-standard options: attempt to group attributes

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(dt_cls_fit, data_test),
    predict(dt_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class2             0.233        0.767
      ##  2 Class1             0.847        0.153
      ##  3 Class1             0.847        0.153
      ##  4 Class1             0.727        0.273
      ##  5 Class1             0.727        0.273
      ##  6 Class2             0.118        0.882
      ##  7 Class2             0.233        0.767
      ##  8 Class2             0.233        0.767
      ##  9 Class1             0.847        0.153
      ## 10 Class2             0.118        0.882

## `gen_additive_mod()` models

With the `"mgcv"` engine

### Regression Example (`mgcv`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  gam_reg_spec <- 
    gen_additive_mod(select_features = FALSE, adjust_deg_free = 10) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("regression") |> 
    set_engine("mgcv")
  gam_reg_spec
```

      ## GAM Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   select_features = FALSE
      ##   adjust_deg_free = 10
      ## 
      ## Computational engine: mgcv

Now we create the model fit object:

``` r
  set.seed(1)
  gam_reg_fit <- gam_reg_spec |> 
    fit(ridership ~ Clark_Lake + Quincy_Wells, data = Chicago_train)
  gam_reg_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Family: gaussian 
      ## Link function: identity 
      ## 
      ## Formula:
      ## ridership ~ Clark_Lake + Quincy_Wells
      ## Total model degrees of freedom 3 
      ## 
      ## GCV score: 9.505245

The holdout data can be predicted:

``` r
  predict(gam_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 20.3 
      ## 2 20.5 
      ## 3 20.8 
      ## 4 20.5 
      ## 5 18.8 
      ## 6  7.45
      ## 7  7.02

### Classification Example (`mgcv`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  gam_cls_spec <- 
    gen_additive_mod(select_features = FALSE, adjust_deg_free = 10) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("classification") |> 
    set_engine("mgcv")
  gam_cls_spec
```

      ## GAM Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   select_features = FALSE
      ##   adjust_deg_free = 10
      ## 
      ## Computational engine: mgcv

Now we create the model fit object:

``` r
  set.seed(1)
  gam_cls_fit <- gam_cls_spec |> fit(Class ~ A + B, data = data_train)
  gam_cls_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Family: binomial 
      ## Link function: logit 
      ## 
      ## Formula:
      ## Class ~ A + B
      ## Total model degrees of freedom 3 
      ## 
      ## UBRE score: -0.07548008

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(gam_cls_fit, data_test),
    predict(gam_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class1             0.518      0.482  
      ##  2 Class1             0.909      0.0913 
      ##  3 Class1             0.648      0.352  
      ##  4 Class1             0.610      0.390  
      ##  5 Class2             0.443      0.557  
      ##  6 Class2             0.206      0.794  
      ##  7 Class1             0.708      0.292  
      ##  8 Class1             0.567      0.433  
      ##  9 Class1             0.994      0.00582
      ## 10 Class2             0.108      0.892

## `linear_reg()` models

With the `"lm"` engine

### Regression Example (`lm`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  linreg_reg_spec <- 
    linear_reg() |> 
    set_engine("lm")
  linreg_reg_spec
```

      ## Linear Regression Model Specification (regression)
      ## 
      ## Computational engine: lm

Now we create the model fit object:

``` r
  set.seed(1)
  linreg_reg_fit <- linreg_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  linreg_reg_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Call:
      ## stats::lm(formula = ridership ~ ., data = data)
      ## 
      ## Coefficients:
      ##  (Intercept)    Clark_Lake  Quincy_Wells  
      ##       1.6624        0.7738        0.2557

The holdout data can be predicted:

``` r
  predict(linreg_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 20.3 
      ## 2 20.5 
      ## 3 20.8 
      ## 4 20.5 
      ## 5 18.8 
      ## 6  7.45
      ## 7  7.02

With the `"glm"` engine

### Regression Example (`glm`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  linreg_reg_spec <- 
    linear_reg() |> 
    set_engine("glm")
  linreg_reg_spec
```

      ## Linear Regression Model Specification (regression)
      ## 
      ## Computational engine: glm

Now we create the model fit object:

``` r
  set.seed(1)
  linreg_reg_fit <- linreg_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  linreg_reg_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Call:  stats::glm(formula = ridership ~ ., family = stats::gaussian, 
      ##     data = data)
      ## 
      ## Coefficients:
      ##  (Intercept)    Clark_Lake  Quincy_Wells  
      ##       1.6624        0.7738        0.2557  
      ## 
      ## Degrees of Freedom: 5690 Total (i.e. Null);  5688 Residual
      ## Null Deviance:     245000 
      ## Residual Deviance: 53530   AIC: 28910

The holdout data can be predicted:

``` r
  predict(linreg_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 20.3 
      ## 2 20.5 
      ## 3 20.8 
      ## 4 20.5 
      ## 5 18.8 
      ## 6  7.45
      ## 7  7.02

With the `"glmnet"` engine

### Regression Example (`glmnet`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  linreg_reg_spec <- 
    linear_reg(penalty = 0.1) |> 
    set_engine("glmnet")
  linreg_reg_spec
```

      ## Linear Regression Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   penalty = 0.1
      ## 
      ## Computational engine: glmnet

Now we create the model fit object:

``` r
  set.seed(1)
  linreg_reg_fit <- linreg_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  linreg_reg_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Call:  glmnet::glmnet(x = maybe_matrix(x), y = y, family = "gaussian") 
      ## 
      ##    Df  %Dev Lambda
      ## 1   0  0.00 5.7970
      ## 2   1 13.25 5.2820
      ## 3   1 24.26 4.8130
      ## 4   1 33.40 4.3850
      ## 5   1 40.98 3.9960
      ## 6   1 47.28 3.6410
      ## 7   1 52.51 3.3170
      ## 8   1 56.85 3.0220
      ## 9   1 60.45 2.7540
      ## 10  1 63.44 2.5090
      ## 11  1 65.92 2.2860
      ## 12  1 67.99 2.0830
      ## 13  1 69.70 1.8980
      ## 14  1 71.12 1.7300
      ## 15  1 72.30 1.5760
      ## 16  2 73.29 1.4360
      ## 17  2 74.11 1.3080
      ## 18  2 74.80 1.1920
      ## 19  2 75.37 1.0860
      ## 20  2 75.84 0.9897
      ## 21  2 76.23 0.9018
      ## 22  2 76.56 0.8217
      ## 23  2 76.83 0.7487
      ## 24  2 77.05 0.6822
      ## 25  2 77.24 0.6216
      ## 26  2 77.39 0.5664
      ## 27  2 77.52 0.5160
      ## 28  2 77.63 0.4702
      ## 29  2 77.72 0.4284
      ## 30  2 77.79 0.3904
      ## 31  2 77.85 0.3557
      ## 32  2 77.90 0.3241
      ## 33  2 77.94 0.2953
      ## 34  2 77.98 0.2691
      ## 35  2 78.01 0.2452
      ## 36  2 78.03 0.2234
      ## 37  2 78.05 0.2035
      ## 38  2 78.07 0.1855
      ## 39  2 78.08 0.1690
      ## 40  2 78.09 0.1540
      ## 41  2 78.10 0.1403
      ## 42  2 78.11 0.1278
      ## 43  2 78.12 0.1165
      ## 44  2 78.12 0.1061
      ## 45  2 78.13 0.0967
      ## 46  2 78.13 0.0881
      ## 47  2 78.13 0.0803
      ## 48  2 78.14 0.0732
      ## 49  2 78.14 0.0666
      ## 50  2 78.14 0.0607
      ## 51  2 78.14 0.0553
      ## 52  2 78.14 0.0504
      ## 53  2 78.14 0.0459
      ## 54  2 78.15 0.0419
      ## 55  2 78.15 0.0381

The holdout data can be predicted:

``` r
  predict(linreg_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 20.2 
      ## 2 20.4 
      ## 3 20.7 
      ## 4 20.4 
      ## 5 18.7 
      ## 6  7.57
      ## 7  7.15

With the `"keras"` engine

### Regression Example (`keras`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  linreg_reg_spec <- 
    linear_reg(penalty = 0.1) |> 
    set_engine("keras")
  linreg_reg_spec
```

      ## Linear Regression Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   penalty = 0.1
      ## 
      ## Computational engine: keras

Now we create the model fit object:

``` r
  set.seed(1)
  linreg_reg_fit <- linreg_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  linreg_reg_fit
```

The holdout data can be predicted:

``` r
  predict(linreg_reg_fit, Chicago_test)
```

With the `"stan"` engine

### Regression Example (`stan`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  linreg_reg_spec <- 
    linear_reg() |> 
    set_engine("stan")
  linreg_reg_spec
```

      ## Linear Regression Model Specification (regression)
      ## 
      ## Computational engine: stan

Now we create the model fit object:

``` r
  set.seed(1)
  linreg_reg_fit <- linreg_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  linreg_reg_fit
```

      ## parsnip model object
      ## 
      ## stan_glm
      ##  family:       gaussian [identity]
      ##  formula:      ridership ~ .
      ##  observations: 5691
      ##  predictors:   3
      ## ------
      ##              Median MAD_SD
      ## (Intercept)  1.7    0.1   
      ## Clark_Lake   0.8    0.0   
      ## Quincy_Wells 0.3    0.1   
      ## 
      ## Auxiliary parameter(s):
      ##       Median MAD_SD
      ## sigma 3.1    0.0   
      ## 
      ## ------
      ## * For help interpreting the printed output see ?print.stanreg
      ## * For info on the priors used see ?prior_summary.stanreg

The holdout data can be predicted:

``` r
  predict(linreg_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 20.3 
      ## 2 20.5 
      ## 3 20.8 
      ## 4 20.5 
      ## 5 18.8 
      ## 6  7.45
      ## 7  7.02

With the `"quantreg"` engine

### Quantile regression Example (`quantreg`)

We’ll model the relationship between the cost of a house in Sacramento
CA and the square footage of a property. A few rows were randomly held
out for illustrating prediction.

``` r
  library(tidymodels)
  tidymodels_prefer()
  
  sac_holdout <- c(90L, 203L, 264L, 733L, 771L)
  sac_train <- Sacramento[-sac_holdout, ]
  sac_test  <- Sacramento[ sac_holdout, ]
```

We can define the model but should set the model mode. Also, for these
models the levels of the distirunbtion that we would like to predict
need to specified with the mode using the `quantile_levels` argument.
Let’s predict the 0.25, 0.50, and 0.75 quantiles:

``` r
  linreg_quant_spec <- 
    linear_reg() |> 
    set_engine("quantreg") |> 
    set_mode("quantile regression", quantile_levels = (1:3) / 4)
  linreg_quant_spec
```

      ## Linear Regression Model Specification (quantile regression)
      ## 
      ## Computational engine: quantreg

      ## Quantile levels: 0.25, 0.5, and 0.75.

Now we create the model fit object:

``` r
  set.seed(1)
  linreg_quant_fit <- linreg_quant_spec |> fit(price ~ sqft, data = sac_train)
  linreg_quant_fit
```

      ## parsnip model object
      ## 
      ## Call:
      ## quantreg::rq(formula = price ~ sqft, tau = quantile_levels, data = data)
      ## 
      ## Coefficients:
      ##              tau= 0.25 tau= 0.50  tau= 0.75
      ## (Intercept) -8492.1136  6124.895 19605.2632
      ## sqft          116.7192   135.865   159.2798
      ## 
      ## Degrees of freedom: 927 total; 925 residual

The holdout data can be predicted:

``` r
  quant_pred <- predict(linreg_quant_fit, sac_test)
  quant_pred
```

      ## # A tibble: 5 × 1
      ##   .pred_quantile
      ##        <qtls(3)>
      ## 1       [206000]
      ## 2       [156000]
      ## 3       [246000]
      ## 4       [284000]
      ## 5       [208000]

`.pred_quantile` is a vector type that contains all of the quartile
predictions for each row. You can convert this to a rectangular data set
using either of:

``` r
  as.matrix(quant_pred$.pred_quantile)
```

      ##          [,1]     [,2]     [,3]
      ## [1,] 163435.3 206254.0 254224.4
      ## [2,] 120365.9 156119.8 195450.1
      ## [3,] 197867.5 246334.2 301211.9
      ## [4,] 230548.9 284376.4 345810.2
      ## [5,] 164836.0 207884.4 256135.7

``` r
  # or 
  as_tibble(quant_pred$.pred_quantile)
```

      ## # A tibble: 15 × 3
      ##    .pred_quantile .quantile_levels  .row
      ##             <dbl>            <dbl> <int>
      ##  1        163435.             0.25     1
      ##  2        206254.             0.5      1
      ##  3        254224.             0.75     1
      ##  4        120366.             0.25     2
      ##  5        156120.             0.5      2
      ##  6        195450.             0.75     2
      ##  7        197868.             0.25     3
      ##  8        246334.             0.5      3
      ##  9        301212.             0.75     3
      ## 10        230549.             0.25     4
      ## 11        284376.             0.5      4
      ## 12        345810.             0.75     4
      ## 13        164836.             0.25     5
      ## 14        207884.             0.5      5
      ## 15        256136.             0.75     5

With the `"brulee"` engine

### Regression Example (`brulee`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  linreg_reg_spec <- 
    linear_reg() |> 
    set_engine("brulee")
  linreg_reg_spec
```

      ## Linear Regression Model Specification (regression)
      ## 
      ## Computational engine: brulee

Now we create the model fit object:

``` r
  set.seed(1)
  linreg_reg_fit <- linreg_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  linreg_reg_fit
```

      ## parsnip model object
      ## 
      ## Linear regression
      ## 
      ## 5,691 samples, 2 features, numeric outcome 
      ## weight decay: 0.001 
      ## batch size: 5122 
      ## scaled validation loss after 2 epochs: 9.73

The holdout data can be predicted:

``` r
  predict(linreg_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 20.4 
      ## 2 20.5 
      ## 3 20.8 
      ## 4 20.6 
      ## 5 18.9 
      ## 6  7.48
      ## 7  7.05

## `logistic_reg()` models

With the `"glm"` engine

### Classification Example (`glm`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  logreg_cls_spec <- 
    logistic_reg() |> 
    set_engine("glm")
  logreg_cls_spec
```

      ## Logistic Regression Model Specification (classification)
      ## 
      ## Computational engine: glm

Now we create the model fit object:

``` r
  set.seed(1)
  logreg_cls_fit <- logreg_cls_spec |> fit(Class ~ ., data = data_train)
  logreg_cls_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Call:  stats::glm(formula = Class ~ ., family = stats::binomial, data = data)
      ## 
      ## Coefficients:
      ## (Intercept)            A            B  
      ##      -3.755       -1.259        3.855  
      ## 
      ## Degrees of Freedom: 780 Total (i.e. Null);  778 Residual
      ## Null Deviance:     1073 
      ## Residual Deviance: 662.1   AIC: 668.1

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(logreg_cls_fit, data_test),
    predict(logreg_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class1             0.518      0.482  
      ##  2 Class1             0.909      0.0913 
      ##  3 Class1             0.648      0.352  
      ##  4 Class1             0.610      0.390  
      ##  5 Class2             0.443      0.557  
      ##  6 Class2             0.206      0.794  
      ##  7 Class1             0.708      0.292  
      ##  8 Class1             0.567      0.433  
      ##  9 Class1             0.994      0.00582
      ## 10 Class2             0.108      0.892

With the `"glmnet"` engine

### Classification Example (`glmnet`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  logreg_cls_spec <- 
    logistic_reg(penalty = 0.1) |> 
    set_engine("glmnet")
  logreg_cls_spec
```

      ## Logistic Regression Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   penalty = 0.1
      ## 
      ## Computational engine: glmnet

Now we create the model fit object:

``` r
  set.seed(1)
  logreg_cls_fit <- logreg_cls_spec |> fit(Class ~ ., data = data_train)
  logreg_cls_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Call:  glmnet::glmnet(x = maybe_matrix(x), y = y, family = "binomial") 
      ## 
      ##    Df  %Dev   Lambda
      ## 1   0  0.00 0.308500
      ## 2   1  4.76 0.281100
      ## 3   1  8.75 0.256100
      ## 4   1 12.13 0.233300
      ## 5   1 15.01 0.212600
      ## 6   1 17.50 0.193700
      ## 7   1 19.64 0.176500
      ## 8   1 21.49 0.160800
      ## 9   1 23.10 0.146500
      ## 10  1 24.49 0.133500
      ## 11  1 25.71 0.121700
      ## 12  1 26.76 0.110900
      ## 13  1 27.67 0.101000
      ## 14  1 28.46 0.092030
      ## 15  1 29.15 0.083860
      ## 16  1 29.74 0.076410
      ## 17  1 30.25 0.069620
      ## 18  1 30.70 0.063430
      ## 19  1 31.08 0.057800
      ## 20  1 31.40 0.052660
      ## 21  1 31.68 0.047990
      ## 22  1 31.92 0.043720
      ## 23  1 32.13 0.039840
      ## 24  2 32.70 0.036300
      ## 25  2 33.50 0.033070
      ## 26  2 34.18 0.030140
      ## 27  2 34.78 0.027460
      ## 28  2 35.29 0.025020
      ## 29  2 35.72 0.022800
      ## 30  2 36.11 0.020770
      ## 31  2 36.43 0.018930
      ## 32  2 36.71 0.017250
      ## 33  2 36.96 0.015710
      ## 34  2 37.16 0.014320
      ## 35  2 37.34 0.013050
      ## 36  2 37.49 0.011890
      ## 37  2 37.62 0.010830
      ## 38  2 37.73 0.009868
      ## 39  2 37.82 0.008992
      ## 40  2 37.90 0.008193
      ## 41  2 37.97 0.007465
      ## 42  2 38.02 0.006802
      ## 43  2 38.07 0.006198
      ## 44  2 38.11 0.005647
      ## 45  2 38.15 0.005145
      ## 46  2 38.18 0.004688
      ## 47  2 38.20 0.004272
      ## 48  2 38.22 0.003892
      ## 49  2 38.24 0.003547
      ## 50  2 38.25 0.003231
      ## 51  2 38.26 0.002944
      ## 52  2 38.27 0.002683
      ## 53  2 38.28 0.002444
      ## 54  2 38.29 0.002227
      ## 55  2 38.29 0.002029
      ## 56  2 38.30 0.001849
      ## 57  2 38.30 0.001685
      ## 58  2 38.31 0.001535
      ## 59  2 38.31 0.001399
      ## 60  2 38.31 0.001275
      ## 61  2 38.31 0.001161
      ## 62  2 38.32 0.001058
      ## 63  2 38.32 0.000964
      ## 64  2 38.32 0.000879
      ## 65  2 38.32 0.000800

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(logreg_cls_fit, data_test),
    predict(logreg_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class1             0.530        0.470
      ##  2 Class1             0.713        0.287
      ##  3 Class1             0.616        0.384
      ##  4 Class2             0.416        0.584
      ##  5 Class2             0.417        0.583
      ##  6 Class2             0.288        0.712
      ##  7 Class1             0.554        0.446
      ##  8 Class1             0.557        0.443
      ##  9 Class1             0.820        0.180
      ## 10 Class2             0.206        0.794

With the `"keras"` engine

### Classification Example (`keras`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  logreg_cls_spec <- 
    logistic_reg(penalty = 0.1) |> 
    set_engine("keras")
  logreg_cls_spec
```

      ## Logistic Regression Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   penalty = 0.1
      ## 
      ## Computational engine: keras

Now we create the model fit object:

``` r
  set.seed(1)
  logreg_cls_fit <- logreg_cls_spec |> fit(Class ~ ., data = data_train)
  logreg_cls_fit
```

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(logreg_cls_fit, data_test),
    predict(logreg_cls_fit, data_test, type = "prob")
  )
```

With the `"LiblineaR"` engine

### Classification Example (`LiblineaR`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  logreg_cls_spec <- 
    logistic_reg(penalty = 0.1) |> 
    set_engine("LiblineaR")
  logreg_cls_spec
```

      ## Logistic Regression Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   penalty = 0.1
      ## 
      ## Computational engine: LiblineaR

Now we create the model fit object:

``` r
  set.seed(1)
  logreg_cls_fit <- logreg_cls_spec |> fit(Class ~ ., data = data_train)
  logreg_cls_fit
```

      ## parsnip model object
      ## 
      ## $TypeDetail
      ## [1] "L2-regularized logistic regression primal (L2R_LR)"
      ## 
      ## $Type
      ## [1] 0
      ## 
      ## $W
      ##             A         B     Bias
      ## [1,] 1.219818 -3.759034 3.674861
      ## 
      ## $Bias
      ## [1] 1
      ## 
      ## $ClassNames
      ## [1] Class1 Class2
      ## Levels: Class1 Class2
      ## 
      ## $NbClass
      ## [1] 2
      ## 
      ## attr(,"class")
      ## [1] "LiblineaR"

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(logreg_cls_fit, data_test),
    predict(logreg_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class1             0.517      0.483  
      ##  2 Class1             0.904      0.0964 
      ##  3 Class1             0.645      0.355  
      ##  4 Class1             0.604      0.396  
      ##  5 Class2             0.442      0.558  
      ##  6 Class2             0.210      0.790  
      ##  7 Class1             0.702      0.298  
      ##  8 Class1             0.565      0.435  
      ##  9 Class1             0.993      0.00667
      ## 10 Class2             0.112      0.888

With the `"stan"` engine

### Classification Example (`stan`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  logreg_cls_spec <- 
    logistic_reg() |> 
    set_engine("stan")
  logreg_cls_spec
```

      ## Logistic Regression Model Specification (classification)
      ## 
      ## Computational engine: stan

Now we create the model fit object:

``` r
  set.seed(1)
  logreg_cls_fit <- logreg_cls_spec |> fit(Class ~ ., data = data_train)
  logreg_cls_fit
```

      ## parsnip model object
      ## 
      ## stan_glm
      ##  family:       binomial [logit]
      ##  formula:      Class ~ .
      ##  observations: 781
      ##  predictors:   3
      ## ------
      ##             Median MAD_SD
      ## (Intercept) -3.8    0.3  
      ## A           -1.3    0.2  
      ## B            3.9    0.3  
      ## 
      ## ------
      ## * For help interpreting the printed output see ?print.stanreg
      ## * For info on the priors used see ?prior_summary.stanreg

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(logreg_cls_fit, data_test),
    predict(logreg_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class1             0.518      0.482  
      ##  2 Class1             0.909      0.0909 
      ##  3 Class1             0.650      0.350  
      ##  4 Class1             0.609      0.391  
      ##  5 Class2             0.443      0.557  
      ##  6 Class2             0.206      0.794  
      ##  7 Class1             0.708      0.292  
      ##  8 Class1             0.568      0.432  
      ##  9 Class1             0.994      0.00580
      ## 10 Class2             0.108      0.892

With the `"brulee"` engine

### Classification Example (`brulee`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  logreg_cls_spec <- 
    logistic_reg() |> 
    set_engine("brulee")
  logreg_cls_spec
```

      ## Logistic Regression Model Specification (classification)
      ## 
      ## Computational engine: brulee

Now we create the model fit object:

``` r
  set.seed(1)
  logreg_cls_fit <- logreg_cls_spec |> fit(Class ~ ., data = data_train)
  logreg_cls_fit
```

      ## parsnip model object
      ## 
      ## Logistic regression
      ## 
      ## 781 samples, 2 features, 2 classes 
      ## class weights Class1=1, Class2=1 
      ## weight decay: 0.001 
      ## batch size: 703 
      ## validation loss after 2 epochs: 0.449

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(logreg_cls_fit, data_test),
    predict(logreg_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class1             0.514      0.486  
      ##  2 Class1             0.907      0.0933 
      ##  3 Class1             0.648      0.352  
      ##  4 Class1             0.594      0.406  
      ##  5 Class2             0.432      0.568  
      ##  6 Class2             0.197      0.803  
      ##  7 Class1             0.701      0.299  
      ##  8 Class1             0.564      0.436  
      ##  9 Class1             0.994      0.00622
      ## 10 Class2             0.102      0.898

## `mars()` models

With the `"earth"` engine

### Regression Example (`earth`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  mars_reg_spec <- 
    mars(prod_degree = 1, prune_method = "backward") |> 
    # This model can be used for classification or regression, so set mode
    set_mode("regression") |> 
    set_engine("earth")
  mars_reg_spec
```

      ## MARS Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   prod_degree = 1
      ##   prune_method = backward
      ## 
      ## Computational engine: earth

Now we create the model fit object:

``` r
  set.seed(1)
  mars_reg_fit <- mars_reg_spec |> fit(ridership ~ ., data = Chicago_train)
```

      ## 
      ## Attaching package: 'plotrix'

      ## The following object is masked from 'package:scales':
      ## 
      ##     rescale

``` r
  mars_reg_fit
```

      ## parsnip model object
      ## 
      ## Selected 5 of 6 terms, and 2 of 2 predictors
      ## Termination condition: RSq changed by less than 0.001 at 6 terms
      ## Importance: Clark_Lake, Quincy_Wells
      ## Number of terms at each degree of interaction: 1 4 (additive model)
      ## GCV 9.085818    RSS 51543.98    GRSq 0.7889881    RSq 0.789581

The holdout data can be predicted:

``` r
  predict(mars_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 20.4 
      ## 2 20.7 
      ## 3 21.0 
      ## 4 20.7 
      ## 5 19.0 
      ## 6  7.99
      ## 7  6.68

### Classification Example (`earth`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  mars_cls_spec <- 
    mars(prod_degree = 1, prune_method = "backward") |> 
    # This model can be used for classification or regression, so set mode
    set_mode("classification") |> 
    set_engine("earth")
  mars_cls_spec
```

      ## MARS Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   prod_degree = 1
      ##   prune_method = backward
      ## 
      ## Computational engine: earth

Now we create the model fit object:

``` r
  set.seed(1)
  mars_cls_fit <- mars_cls_spec |> fit(Class ~ ., data = data_train)
  mars_cls_fit
```

      ## parsnip model object
      ## 
      ## GLM (family binomial, link logit):
      ##  nulldev  df       dev  df   devratio     AIC iters converged
      ##  1073.43 780   632.723 775      0.411   644.7     5         1
      ## 
      ## Earth selected 6 of 13 terms, and 2 of 2 predictors
      ## Termination condition: Reached nk 21
      ## Importance: B, A
      ## Number of terms at each degree of interaction: 1 5 (additive model)
      ## Earth GCV 0.1334948    RSS 101.3432    GRSq 0.461003    RSq 0.4747349

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(mars_cls_fit, data_test),
    predict(mars_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class2            0.332       0.668  
      ##  2 Class1            0.845       0.155  
      ##  3 Class1            0.585       0.415  
      ##  4 Class1            0.690       0.310  
      ##  5 Class2            0.483       0.517  
      ##  6 Class2            0.318       0.682  
      ##  7 Class1            0.661       0.339  
      ##  8 Class2            0.398       0.602  
      ##  9 Class1            0.990       0.00972
      ## 10 Class2            0.0625      0.938

## `mlp()` models

With the `"nnet"` engine

### Regression Example (`nnet`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  mlp_reg_spec <- 
    mlp(penalty = 0, epochs = 100) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("regression") |> 
    set_engine("nnet")
  mlp_reg_spec
```

      ## Single Layer Neural Network Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   penalty = 0
      ##   epochs = 100
      ## 
      ## Computational engine: nnet

Now we create the model fit object:

``` r
  set.seed(1)
  mlp_reg_fit <- mlp_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  mlp_reg_fit
```

      ## parsnip model object
      ## 
      ## a 2-5-1 network with 21 weights
      ## inputs: Clark_Lake Quincy_Wells 
      ## output(s): ridership 
      ## options were - linear output units

The holdout data can be predicted:

``` r
  predict(mlp_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 20.5 
      ## 2 20.8 
      ## 3 21.1 
      ## 4 20.8 
      ## 5 18.8 
      ## 6  8.09
      ## 7  6.22

### Classification Example (`nnet`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  mlp_cls_spec <- 
    mlp(penalty = 0, epochs = 100) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("classification") |> 
    set_engine("nnet")
  mlp_cls_spec
```

      ## Single Layer Neural Network Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   penalty = 0
      ##   epochs = 100
      ## 
      ## Computational engine: nnet

Now we create the model fit object:

``` r
  set.seed(1)
  mlp_cls_fit <- mlp_cls_spec |> fit(Class ~ ., data = data_train)
  mlp_cls_fit
```

      ## parsnip model object
      ## 
      ## a 2-5-1 network with 21 weights
      ## inputs: A B 
      ## output(s): Class 
      ## options were - entropy fitting

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(mlp_cls_fit, data_test),
    predict(mlp_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class2             0.364        0.636
      ##  2 Class1             0.691        0.309
      ##  3 Class1             0.577        0.423
      ##  4 Class1             0.686        0.314
      ##  5 Class2             0.466        0.534
      ##  6 Class2             0.339        0.661
      ##  7 Class1             0.670        0.330
      ##  8 Class2             0.384        0.616
      ##  9 Class1             0.692        0.308
      ## 10 Class2             0.330        0.670

With the `"keras"` engine

### Regression Example (`keras`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  mlp_reg_spec <- 
    mlp(penalty = 0, epochs = 20) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("regression") |> 
    set_engine("keras")
  mlp_reg_spec
```

      ## Single Layer Neural Network Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   penalty = 0
      ##   epochs = 20
      ## 
      ## Computational engine: keras

Now we create the model fit object:

``` r
  set.seed(1)
  mlp_reg_fit <- mlp_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  mlp_reg_fit
```

The holdout data can be predicted:

``` r
  predict(mlp_reg_fit, Chicago_test)
```

### Classification Example (`keras`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  mlp_cls_spec <- 
    mlp(penalty = 0, epochs = 20) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("classification") |> 
    set_engine("keras")
  mlp_cls_spec
```

      ## Single Layer Neural Network Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   penalty = 0
      ##   epochs = 20
      ## 
      ## Computational engine: keras

Now we create the model fit object:

``` r
  set.seed(1)
  mlp_cls_fit <- mlp_cls_spec |> fit(Class ~ ., data = data_train)
  mlp_cls_fit
```

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(mlp_cls_fit, data_test),
    predict(mlp_cls_fit, data_test, type = "prob")
  )
```

With the `"brulee"` engine

### Regression Example (`brulee`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  mlp_reg_spec <- 
    mlp(penalty = 0, epochs = 100) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("regression") |> 
    set_engine("brulee")
  mlp_reg_spec
```

      ## Single Layer Neural Network Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   penalty = 0
      ##   epochs = 100
      ## 
      ## Computational engine: brulee

Now we create the model fit object:

``` r
  set.seed(1)
  mlp_reg_fit <- mlp_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  mlp_reg_fit
```

      ## parsnip model object
      ## 
      ## Multilayer perceptron
      ## 
      ## relu activation,
      ## 3 hidden units,
      ## 13 model parameters
      ## 5,691 samples, 2 features, numeric outcome 
      ## dropout proportion: 0 
      ## batch size: 5122 
      ## learn rate: 0.01 
      ## scaled validation loss after 0 epochs:

The holdout data can be predicted:

``` r
  predict(mlp_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1  48.3
      ## 2  48.9
      ## 3  49.3
      ## 4  48.8
      ## 5  46.4
      ## 6  28.5
      ## 7  28.4

### Classification Example (`brulee`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  mlp_cls_spec <- 
    mlp(penalty = 0, epochs = 100) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("classification") |> 
    set_engine("brulee")
  mlp_cls_spec
```

      ## Single Layer Neural Network Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   penalty = 0
      ##   epochs = 100
      ## 
      ## Computational engine: brulee

Now we create the model fit object:

``` r
  set.seed(1)
  mlp_cls_fit <- mlp_cls_spec |> fit(Class ~ ., data = data_train)
  mlp_cls_fit
```

      ## parsnip model object
      ## 
      ## Multilayer perceptron
      ## 
      ## relu activation,
      ## 3 hidden units,
      ## 17 model parameters
      ## 781 samples, 2 features, 2 classes 
      ## class weights Class1=1, Class2=1 
      ## dropout proportion: 0 
      ## batch size: 703 
      ## learn rate: 0.01 
      ## validation loss after 17 epochs: 0.443

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(mlp_cls_fit, data_test),
    predict(mlp_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class1             0.545       0.455 
      ##  2 Class1             0.914       0.0860
      ##  3 Class1             0.676       0.324 
      ##  4 Class1             0.675       0.325 
      ##  5 Class2             0.483       0.517 
      ##  6 Class2             0.223       0.777 
      ##  7 Class1             0.753       0.247 
      ##  8 Class1             0.596       0.404 
      ##  9 Class1             0.975       0.0251
      ## 10 Class2             0.115       0.885

With the `"brulee_two_layer"` engine

### Regression Example (`brulee_two_layer`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  mlp_reg_spec <- 
    mlp(penalty = 0, epochs = 10) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("regression") |> 
    set_engine("brulee_two_layer", hidden_units_2 = 2)
  mlp_reg_spec
```

      ## Single Layer Neural Network Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   penalty = 0
      ##   epochs = 10
      ## 
      ## Engine-Specific Arguments:
      ##   hidden_units_2 = 2
      ## 
      ## Computational engine: brulee_two_layer

Now we create the model fit object:

``` r
  set.seed(13)
  mlp_reg_fit <- mlp_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  mlp_reg_fit
```

      ## parsnip model object
      ## 
      ## Multilayer perceptron
      ## 
      ## c(relu,relu) activation,
      ## c(3,2) hidden units,
      ## 20 model parameters
      ## 5,691 samples, 2 features, numeric outcome 
      ## dropout proportion: 0 
      ## batch size: 5122 
      ## learn rate: 0.01 
      ## scaled validation loss after 10 epochs: 1.03

The holdout data can be predicted:

``` r
  predict(mlp_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1  13.8
      ## 2  13.8
      ## 3  13.8
      ## 4  13.8
      ## 5  13.8
      ## 6  13.8
      ## 7  13.8

### Classification Example (`brulee_two_layer`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  mlp_cls_spec <- 
    mlp(penalty = 0, epochs = 10) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("classification") |> 
    set_engine("brulee_two_layer", hidden_units_2 = 2)
  mlp_cls_spec
```

      ## Single Layer Neural Network Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   penalty = 0
      ##   epochs = 10
      ## 
      ## Engine-Specific Arguments:
      ##   hidden_units_2 = 2
      ## 
      ## Computational engine: brulee_two_layer

Now we create the model fit object:

``` r
  set.seed(12)
  mlp_cls_fit <- mlp_cls_spec |> fit(Class ~ ., data = data_train)
  mlp_cls_fit
```

      ## parsnip model object
      ## 
      ## Multilayer perceptron
      ## 
      ## c(relu,relu) activation,
      ## c(3,2) hidden units,
      ## 23 model parameters
      ## 781 samples, 2 features, 2 classes 
      ## class weights Class1=1, Class2=1 
      ## dropout proportion: 0 
      ## batch size: 703 
      ## learn rate: 0.01 
      ## validation loss after 10 epochs: 0.354

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(mlp_cls_fit, data_test),
    predict(mlp_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class1            0.517        0.483 
      ##  2 Class1            0.928        0.0717
      ##  3 Class1            0.663        0.337 
      ##  4 Class1            0.716        0.284 
      ##  5 Class2            0.473        0.527 
      ##  6 Class2            0.190        0.810 
      ##  7 Class1            0.776        0.224 
      ##  8 Class1            0.575        0.425 
      ##  9 Class1            0.931        0.0690
      ## 10 Class2            0.0978       0.902

## `multinom_reg()` models

With the `"glmnet"` engine

### Classification Example (`glmnet`)

We’ll predict the island where the penguins were observed with two
variables in the same unit (mm): bill length and bill depth.

``` r
library(tidymodels)
tidymodels_prefer()
data(penguins)

penguins <- penguins |> select(island, starts_with("bill_"))
penguins_train <- penguins[-c(21, 153, 31, 277, 1), ]
penguins_test  <- penguins[ c(21, 153, 31, 277, 1), ]
```

We can define the model with specific parameters:

``` r
mr_cls_spec <- 
  multinom_reg(penalty = 0.1) |> 
  set_engine("glmnet")
mr_cls_spec
```

    ## Multinomial Regression Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   penalty = 0.1
    ## 
    ## Computational engine: glmnet

Now we create the model fit object:

``` r
set.seed(1)
mr_cls_fit <- mr_cls_spec |> fit(island ~ ., data = penguins_train)
mr_cls_fit
```

    ## parsnip model object
    ## 
    ## 
    ## Call:  glmnet::glmnet(x = maybe_matrix(x), y = y, family = "multinomial") 
    ## 
    ##    Df  %Dev  Lambda
    ## 1   0  0.00 0.31730
    ## 2   1  3.43 0.28910
    ## 3   1  6.30 0.26340
    ## 4   1  8.74 0.24000
    ## 5   1 10.83 0.21870
    ## 6   1 12.62 0.19930
    ## 7   1 14.17 0.18160
    ## 8   1 15.51 0.16540
    ## 9   1 16.67 0.15070
    ## 10  1 17.68 0.13740
    ## 11  1 18.56 0.12520
    ## 12  2 19.93 0.11400
    ## 13  2 21.31 0.10390
    ## 14  2 22.50 0.09467
    ## 15  2 23.52 0.08626
    ## 16  2 24.40 0.07860
    ## 17  2 25.16 0.07162
    ## 18  2 25.81 0.06526
    ## 19  2 26.37 0.05946
    ## 20  2 26.86 0.05418
    ## 21  2 27.27 0.04936
    ## 22  2 27.63 0.04498
    ## 23  2 27.94 0.04098
    ## 24  2 28.21 0.03734
    ## 25  2 28.44 0.03402
    ## 26  2 28.63 0.03100
    ## 27  2 28.80 0.02825
    ## 28  2 28.94 0.02574
    ## 29  2 29.06 0.02345
    ## 30  2 29.17 0.02137
    ## 31  2 29.26 0.01947
    ## 32  2 29.33 0.01774
    ## 33  2 29.39 0.01616
    ## 34  2 29.45 0.01473
    ## 35  2 29.49 0.01342
    ## 36  2 29.53 0.01223
    ## 37  2 29.56 0.01114
    ## 38  2 29.59 0.01015
    ## 39  2 29.61 0.00925
    ## 40  2 29.63 0.00843
    ## 41  2 29.65 0.00768
    ## 42  2 29.67 0.00700
    ## 43  2 29.68 0.00638
    ## 44  2 29.69 0.00581
    ## 45  2 29.70 0.00529
    ## 46  2 29.71 0.00482
    ## 47  2 29.71 0.00439
    ## 48  2 29.72 0.00400
    ## 49  2 29.72 0.00365
    ## 50  2 29.73 0.00332
    ## 51  2 29.73 0.00303
    ## 52  2 29.74 0.00276
    ## 53  2 29.74 0.00251
    ## 54  2 29.74 0.00229
    ## 55  2 29.75 0.00209
    ## 56  2 29.75 0.00190
    ## 57  2 29.75 0.00173
    ## 58  2 29.75 0.00158
    ## 59  2 29.75 0.00144
    ## 60  2 29.75 0.00131

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
bind_cols(
  predict(mr_cls_fit, penguins_test),
  predict(mr_cls_fit, penguins_test, type = "prob")
)
```

    ## # A tibble: 5 × 4
    ##   .pred_class .pred_Biscoe .pred_Dream .pred_Torgersen
    ##   <fct>              <dbl>       <dbl>           <dbl>
    ## 1 Dream              0.339      0.448           0.214 
    ## 2 Biscoe             0.879      0.0882          0.0331
    ## 3 Biscoe             0.539      0.317           0.144 
    ## 4 Dream              0.403      0.435           0.162 
    ## 5 Dream              0.297      0.481           0.221

With the `"keras"` engine

### Classification Example (`keras`)

We’ll predict the island where the penguins were observed with two
variables in the same unit (mm): bill length and bill depth.

``` r
library(tidymodels)
tidymodels_prefer()
data(penguins)

penguins <- penguins |> select(island, starts_with("bill_"))
penguins_train <- penguins[-c(21, 153, 31, 277, 1), ]
penguins_test  <- penguins[ c(21, 153, 31, 277, 1), ]
```

We can define the model with specific parameters:

``` r
mr_cls_spec <- 
  multinom_reg(penalty = 0.1) |> 
  set_engine("keras")
mr_cls_spec
```

    ## Multinomial Regression Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   penalty = 0.1
    ## 
    ## Computational engine: keras

Now we create the model fit object:

``` r
set.seed(1)
mr_cls_fit <- mr_cls_spec |> fit(island ~ ., data = penguins_train)
mr_cls_fit
```

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
bind_cols(
  predict(mr_cls_fit, penguins_test),
  predict(mr_cls_fit, penguins_test, type = "prob")
)
```

With the `"nnet"` engine

### Classification Example (`nnet`)

We’ll predict the island where the penguins were observed with two
variables in the same unit (mm): bill length and bill depth.

``` r
library(tidymodels)
tidymodels_prefer()
data(penguins)

penguins <- penguins |> select(island, starts_with("bill_"))
penguins_train <- penguins[-c(21, 153, 31, 277, 1), ]
penguins_test  <- penguins[ c(21, 153, 31, 277, 1), ]
```

We can define the model with specific parameters:

``` r
mr_cls_spec <- 
  multinom_reg(penalty = 0.1) |> 
  set_engine("nnet")
mr_cls_spec
```

    ## Multinomial Regression Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   penalty = 0.1
    ## 
    ## Computational engine: nnet

Now we create the model fit object:

``` r
set.seed(1)
mr_cls_fit <- mr_cls_spec |> fit(island ~ ., data = penguins_train)
mr_cls_fit
```

    ## parsnip model object
    ## 
    ## Call:
    ## nnet::multinom(formula = island ~ ., data = data, decay = ~0.1, 
    ##     trace = FALSE)
    ## 
    ## Coefficients:
    ##           (Intercept) bill_length_mm bill_depth_mm
    ## Dream       -8.243575     -0.0580960     0.6168318
    ## Torgersen   -1.610588     -0.2789588     0.6978480
    ## 
    ## Residual Deviance: 502.5009 
    ## AIC: 514.5009

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
bind_cols(
  predict(mr_cls_fit, penguins_test),
  predict(mr_cls_fit, penguins_test, type = "prob")
)
```

    ## # A tibble: 5 × 4
    ##   .pred_class .pred_Biscoe .pred_Dream .pred_Torgersen
    ##   <fct>              <dbl>       <dbl>           <dbl>
    ## 1 Dream              0.193      0.450          0.357  
    ## 2 Biscoe             0.937      0.0582         0.00487
    ## 3 Biscoe             0.462      0.364          0.174  
    ## 4 Dream              0.450      0.495          0.0556 
    ## 5 Dream              0.183      0.506          0.311

With the `"brulee"` engine

### Classification Example (`brulee`)

We’ll predict the island where the penguins were observed with two
variables in the same unit (mm): bill length and bill depth.

``` r
library(tidymodels)
tidymodels_prefer()
data(penguins)

penguins <- penguins |> select(island, starts_with("bill_"))
penguins_train <- penguins[-c(21, 153, 31, 277, 1), ]
penguins_test  <- penguins[ c(21, 153, 31, 277, 1), ]
```

We can define the model with specific parameters:

``` r
mr_cls_spec <- 
  multinom_reg() |> 
  set_engine("brulee", learn_rate = 0.01, optimizer = "SGD")
mr_cls_spec
```

    ## Multinomial Regression Model Specification (classification)
    ## 
    ## Engine-Specific Arguments:
    ##   learn_rate = 0.01
    ##   optimizer = SGD
    ## 
    ## Computational engine: brulee

Now we create the model fit object:

``` r
set.seed(1)
mr_cls_fit <- mr_cls_spec |> fit(island ~ ., data = penguins_train)
mr_cls_fit
```

    ## parsnip model object
    ## 
    ## Multinomial regression
    ## 
    ## 337 samples, 2 features, 3 classes 
    ## class weights Biscoe=1, Dream=1, Torgersen=1 
    ## weight decay: 0.001 
    ## batch size: 304 
    ## validation loss after 1 epoch: 1.72

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
bind_cols(
  predict(mr_cls_fit, penguins_test),
  predict(mr_cls_fit, penguins_test, type = "prob")
)
```

    ## # A tibble: 5 × 4
    ##   .pred_class .pred_Biscoe .pred_Dream .pred_Torgersen
    ##   <fct>              <dbl>       <dbl>           <dbl>
    ## 1 Dream             0.0219       0.978      0.0000324 
    ## 2 Biscoe            0.631        0.369      0.00000606
    ## 3 Dream             0.0694       0.931      0.0000291 
    ## 4 Dream             0.114        0.886      0.00000662
    ## 5 Dream             0.0220       0.978      0.0000242

## `nearest_neighbor()` models

With the `"kknn"` engine

### Regression Example (`kknn`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
library(tidymodels)
tidymodels_prefer()
data(Chicago)

n <- nrow(Chicago)
Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)

Chicago_train <- Chicago[1:(n - 7), ]
Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
knn_reg_spec <-
  nearest_neighbor(neighbors = 5, weight_func = "triangular") |>
  # This model can be used for classification or regression, so set mode
  set_mode("regression") |>
  set_engine("kknn")
knn_reg_spec
```

    ## K-Nearest Neighbor Model Specification (regression)
    ## 
    ## Main Arguments:
    ##   neighbors = 5
    ##   weight_func = triangular
    ## 
    ## Computational engine: kknn

Now we create the model fit object:

``` r
knn_reg_fit <- knn_reg_spec |> fit(ridership ~ ., data = Chicago_train)
knn_reg_fit
```

    ## parsnip model object
    ## 
    ## 
    ## Call:
    ## kknn::train.kknn(formula = ridership ~ ., data = data, ks = min_rows(5,     data, 5), kernel = ~"triangular")
    ## 
    ## Type of response variable: continuous
    ## minimal mean absolute error: 1.79223
    ## Minimal mean squared error: 11.21809
    ## Best kernel: triangular
    ## Best k: 5

The holdout data can be predicted:

``` r
predict(knn_reg_fit, Chicago_test)
```

    ## # A tibble: 7 × 1
    ##   .pred
    ##   <dbl>
    ## 1 20.5 
    ## 2 21.1 
    ## 3 21.4 
    ## 4 21.8 
    ## 5 19.5 
    ## 6  7.83
    ## 7  5.54

### Classification Example (`kknn`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
library(tidymodels)
tidymodels_prefer()
data(two_class_dat)

data_train <- two_class_dat[-(1:10), ]
data_test  <- two_class_dat[  1:10 , ]
```

Since there are two classes, we’ll use an odd number of neighbors to
avoid ties:

``` r
knn_cls_spec <-
  nearest_neighbor(neighbors = 11, weight_func = "triangular") |>
  # This model can be used for classification or regression, so set mode
  set_mode("classification") |>
  set_engine("kknn")
knn_cls_spec
```

    ## K-Nearest Neighbor Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   neighbors = 11
    ##   weight_func = triangular
    ## 
    ## Computational engine: kknn

Now we create the model fit object:

``` r
knn_cls_fit <- knn_cls_spec |> fit(Class ~ ., data = data_train)
knn_cls_fit
```

    ## parsnip model object
    ## 
    ## 
    ## Call:
    ## kknn::train.kknn(formula = Class ~ ., data = data, ks = min_rows(11,     data, 5), kernel = ~"triangular")
    ## 
    ## Type of response variable: nominal
    ## Minimal misclassification: 0.1869398
    ## Best kernel: triangular
    ## Best k: 11

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
bind_cols(
  predict(knn_cls_fit, data_test),
  predict(knn_cls_fit, data_test, type = "prob")
)
```

    ## # A tibble: 10 × 3
    ##    .pred_class .pred_Class1 .pred_Class2
    ##    <fct>              <dbl>        <dbl>
    ##  1 Class2            0.177       0.823  
    ##  2 Class1            0.995       0.00515
    ##  3 Class1            0.590       0.410  
    ##  4 Class1            0.770       0.230  
    ##  5 Class2            0.333       0.667  
    ##  6 Class2            0.182       0.818  
    ##  7 Class1            0.692       0.308  
    ##  8 Class2            0.400       0.600  
    ##  9 Class1            0.814       0.186  
    ## 10 Class2            0.0273      0.973

## `rand_forest()` models

With the `"ranger"` engine

### Regression Example (`ranger`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  rf_reg_spec <- 
    rand_forest(trees = 200, min_n = 5) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("regression") |> 
    set_engine("ranger")
  rf_reg_spec
```

      ## Random Forest Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   trees = 200
      ##   min_n = 5
      ## 
      ## Computational engine: ranger

Now we create the model fit object:

``` r
  set.seed(1)
  rf_reg_fit <- rf_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  rf_reg_fit
```

      ## parsnip model object
      ## 
      ## Ranger result
      ## 
      ## Call:
      ##  ranger::ranger(x = maybe_data_frame(x), y = y, num.trees = ~200,      min.node.size = min_rows(~5, x), num.threads = 1, verbose = FALSE,      seed = sample.int(10^5, 1)) 
      ## 
      ## Type:                             Regression 
      ## Number of trees:                  200 
      ## Sample size:                      5691 
      ## Number of independent variables:  2 
      ## Mtry:                             1 
      ## Target node size:                 5 
      ## Variable importance mode:         none 
      ## Splitrule:                        variance 
      ## OOB prediction error (MSE):       9.72953 
      ## R squared (OOB):                  0.7739986

The holdout data can be predicted:

``` r
  predict(rf_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 20.4 
      ## 2 21.5 
      ## 3 20.8 
      ## 4 21.6 
      ## 5 19.4 
      ## 6  7.32
      ## 7  6.03

### Classification Example (`ranger`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  rf_cls_spec <- 
    rand_forest(trees = 200, min_n = 5) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("classification") |> 
    set_engine("ranger")
  rf_cls_spec
```

      ## Random Forest Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   trees = 200
      ##   min_n = 5
      ## 
      ## Computational engine: ranger

Now we create the model fit object:

``` r
  set.seed(1)
  rf_cls_fit <- rf_cls_spec |> fit(Class ~ ., data = data_train)
  rf_cls_fit
```

      ## parsnip model object
      ## 
      ## Ranger result
      ## 
      ## Call:
      ##  ranger::ranger(x = maybe_data_frame(x), y = y, num.trees = ~200,      min.node.size = min_rows(~5, x), num.threads = 1, verbose = FALSE,      seed = sample.int(10^5, 1), probability = TRUE) 
      ## 
      ## Type:                             Probability estimation 
      ## Number of trees:                  200 
      ## Sample size:                      781 
      ## Number of independent variables:  2 
      ## Mtry:                             1 
      ## Target node size:                 5 
      ## Variable importance mode:         none 
      ## Splitrule:                        gini 
      ## OOB prediction error (Brier s.):  0.1534794

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(rf_cls_fit, data_test),
    predict(rf_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class2           0.274         0.725 
      ##  2 Class1           0.928         0.0716
      ##  3 Class2           0.497         0.503 
      ##  4 Class1           0.703         0.297 
      ##  5 Class2           0.302         0.698 
      ##  6 Class2           0.151         0.849 
      ##  7 Class1           0.701         0.299 
      ##  8 Class1           0.592         0.409 
      ##  9 Class1           0.752         0.248 
      ## 10 Class2           0.00225       0.998

With the `"randomForest"` engine

### Regression Example (`randomForest`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  rf_reg_spec <- 
    rand_forest(trees = 200, min_n = 5) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("regression") |> 
    set_engine("randomForest")
  rf_reg_spec
```

      ## Random Forest Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   trees = 200
      ##   min_n = 5
      ## 
      ## Computational engine: randomForest

Now we create the model fit object:

``` r
  set.seed(1)
  rf_reg_fit <- rf_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  rf_reg_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Call:
      ##  randomForest(x = maybe_data_frame(x), y = y, ntree = ~200, nodesize = min_rows(~5,      x)) 
      ##                Type of random forest: regression
      ##                      Number of trees: 200
      ## No. of variables tried at each split: 1
      ## 
      ##           Mean of squared residuals: 9.696736
      ##                     % Var explained: 77.47

The holdout data can be predicted:

``` r
  predict(rf_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 20.4 
      ## 2 21.6 
      ## 3 20.9 
      ## 4 21.6 
      ## 5 19.3 
      ## 6  7.33
      ## 7  6.16

### Classification Example (`randomForest`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  rf_cls_spec <- 
    rand_forest(trees = 200, min_n = 5) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("classification") |> 
    set_engine("randomForest")
  rf_cls_spec
```

      ## Random Forest Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   trees = 200
      ##   min_n = 5
      ## 
      ## Computational engine: randomForest

Now we create the model fit object:

``` r
  set.seed(1)
  rf_cls_fit <- rf_cls_spec |> fit(Class ~ ., data = data_train)
  rf_cls_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Call:
      ##  randomForest(x = maybe_data_frame(x), y = y, ntree = ~200, nodesize = min_rows(~5,      x)) 
      ##                Type of random forest: classification
      ##                      Number of trees: 200
      ## No. of variables tried at each split: 1
      ## 
      ##         OOB estimate of  error rate: 19.72%
      ## Confusion matrix:
      ##        Class1 Class2 class.error
      ## Class1    363     70   0.1616628
      ## Class2     84    264   0.2413793

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(rf_cls_fit, data_test),
    predict(rf_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class2             0.23         0.77 
      ##  2 Class1             0.95         0.05 
      ##  3 Class1             0.59         0.41 
      ##  4 Class1             0.75         0.25 
      ##  5 Class2             0.305        0.695
      ##  6 Class2             0.105        0.895
      ##  7 Class1             0.685        0.315
      ##  8 Class1             0.63         0.37 
      ##  9 Class1             0.79         0.21 
      ## 10 Class2             0.02         0.98

With the `"grf"` engine

### Regression Example (`grf`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  rf_reg_spec <- 
    rand_forest(trees = 200, min_n = 5) |> 
    # This model can be used for classification, regression, or quantile
    # regression so set mode
    set_mode("regression") |> 
    set_engine("grf")
  rf_reg_spec
```

      ## Random Forest Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   trees = 200
      ##   min_n = 5
      ## 
      ## Computational engine: grf

Now we create the model fit object:

``` r
  set.seed(1)
  rf_reg_fit <- rf_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  rf_reg_fit
```

      ## parsnip model object
      ## 
      ## GRF forest object of type regression_forest 
      ## Number of trees: 200 
      ## Number of training samples: 5691 
      ## Variable importance: 
      ##     1     2 
      ## 0.356 0.644

The holdout data can be predicted for their mean value as well as
confidence intervals for the predictions:

``` r
  predict(rf_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 20.2 
      ## 2 21.0 
      ## 3 21.0 
      ## 4 20.9 
      ## 5 19.4 
      ## 6  7.73
      ## 7  6.48

``` r
  predict(rf_reg_fit, Chicago_test, type = "conf_int")
```

      ## # A tibble: 7 × 2
      ##   .pred_lower .pred_upper
      ##         <dbl>       <dbl>
      ## 1       21.0        19.4 
      ## 2       21.7        20.2 
      ## 3       22.2        19.9 
      ## 4       21.5        20.3 
      ## 5       20.6        18.3 
      ## 6        8.79        6.67
      ## 7        7.23        5.73

### Classification Example (`grf`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  rf_cls_spec <- 
    rand_forest(trees = 200, min_n = 5) |> 
    # This model can be used for classification, regression, or quantile
    # regression so set mode
    set_mode("classification") |> 
    set_engine("grf")
  rf_cls_spec
```

      ## Random Forest Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   trees = 200
      ##   min_n = 5
      ## 
      ## Computational engine: grf

Now we create the model fit object:

``` r
  set.seed(1)
  rf_cls_fit <- rf_cls_spec |> fit(Class ~ ., data = data_train)
  rf_cls_fit
```

      ## parsnip model object
      ## 
      ## GRF forest object of type probability_forest 
      ## Number of trees: 200 
      ## Number of training samples: 781 
      ## Variable importance: 
      ##     1     2 
      ## 0.279 0.721

The holdout data can be predicted for both hard class predictions,
probabilities, and confidence intervals. We’ll bind these together into
one tibble:

``` r
  bind_cols(
    predict(rf_cls_fit, data_test),
    predict(rf_cls_fit, data_test, type = "prob"),
    predict(rf_cls_fit, data_test, type = "conf_int")
  )
```

      ## # A tibble: 10 × 7
      ##    .pred_class .pred_Class1 .pred_Class2 .pred_lower_Class1
      ##    <fct>              <dbl>        <dbl>              <dbl>
      ##  1 Class2            0.283         0.717              0.513
      ##  2 Class1            0.867         0.133              0.958
      ##  3 Class1            0.532         0.468              0.749
      ##  4 Class1            0.618         0.382              0.943
      ##  5 Class2            0.454         0.546              0.826
      ##  6 Class2            0.135         0.865              0.339
      ##  7 Class1            0.621         0.379              0.861
      ##  8 Class2            0.471         0.529              0.622
      ##  9 Class1            0.881         0.119              1.16 
      ## 10 Class2            0.0725        0.927              0.129
      ## # ℹ 3 more variables: .pred_lower_Class2 <dbl>,
      ## #   .pred_upper_Class1 <dbl>, .pred_upper_Class2 <dbl>

### Quantile regression Example (`grf`)

We’ll model the relationship between the cost of a house in Sacramento
CA and the square footage of a property. A few rows were randomly held
out for illustrating prediction.

``` r
  library(tidymodels)
  tidymodels_prefer()
  
  sac_holdout <- c(90L, 203L, 264L, 733L, 771L)
  sac_train <- Sacramento[-sac_holdout, ]
  sac_test  <- Sacramento[ sac_holdout, ]
```

We can define the model but should set the model mode. Also, for these
models the levels of the distirunbtion that we would like to predict
need to specified with the mode using the `quantile_levels` argument.
Let’s predict the 0.25, 0.50, and 0.75 quantiles:

``` r
  grf_quant_spec <- 
    rand_forest() |> 
    set_engine("grf") |> 
    set_mode("quantile regression", quantile_levels = (1:3) / 4)
  grf_quant_spec
```

      ## Random Forest Model Specification (quantile regression)
      ## 
      ## Computational engine: grf

      ## Quantile levels: 0.25, 0.5, and 0.75.

Now we create the model fit object:

``` r
  set.seed(1)
  grf_quant_fit <- grf_quant_spec |> fit(price ~ sqft, data = sac_train)
  grf_quant_fit
```

      ## parsnip model object
      ## 
      ## GRF forest object of type quantile_forest 
      ## Number of trees: 2000 
      ## Number of training samples: 927 
      ## Variable importance: 
      ## 1 
      ## 1

The holdout data can be predicted:

``` r
  quant_pred <- predict(grf_quant_fit, sac_test)
  quant_pred
```

      ## # A tibble: 5 × 1
      ##   .pred_quantile
      ##        <qtls(3)>
      ## 1       [205000]
      ## 2       [145000]
      ## 3       [225000]
      ## 4       [315000]
      ## 5       [198000]

`.pred_quantile` is a vector type that contains all of the quartile
predictions for each row. You can convert this to a rectangular data set
using either of:

``` r
  as.matrix(quant_pred$.pred_quantile)
```

      ##        [,1]   [,2]   [,3]
      ## [1,] 174250 205000 231477
      ## [2,] 120108 145000 170000
      ## [3,] 210000 225000 270000
      ## [4,] 256054 315000 408431
      ## [5,] 168000 197654 245000

``` r
  # or 
  as_tibble(quant_pred$.pred_quantile)
```

      ## # A tibble: 15 × 3
      ##    .pred_quantile .quantile_levels  .row
      ##             <dbl>            <dbl> <int>
      ##  1         174250             0.25     1
      ##  2         205000             0.5      1
      ##  3         231477             0.75     1
      ##  4         120108             0.25     2
      ##  5         145000             0.5      2
      ##  6         170000             0.75     2
      ##  7         210000             0.25     3
      ##  8         225000             0.5      3
      ##  9         270000             0.75     3
      ## 10         256054             0.25     4
      ## 11         315000             0.5      4
      ## 12         408431             0.75     4
      ## 13         168000             0.25     5
      ## 14         197654             0.5      5
      ## 15         245000             0.75     5

## `svm_linear()` models

With the `"LiblineaR"` engine

### Regression Example (`LiblineaR`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  svm_reg_spec <- 
    svm_linear(cost = 1, margin = 0.1) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("regression") |> 
    set_engine("LiblineaR")
  svm_reg_spec
```

      ## Linear Support Vector Machine Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   cost = 1
      ##   margin = 0.1
      ## 
      ## Computational engine: LiblineaR

Now we create the model fit object:

``` r
  set.seed(1)
  svm_reg_fit <- svm_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  svm_reg_fit
```

      ## parsnip model object
      ## 
      ## $TypeDetail
      ## [1] "L2-regularized L2-loss support vector regression primal (L2R_L2LOSS_SVR)"
      ## 
      ## $Type
      ## [1] 11
      ## 
      ## $W
      ##      Clark_Lake Quincy_Wells       Bias
      ## [1,]  0.8277352    0.3430336 0.05042585
      ## 
      ## $Bias
      ## [1] 1
      ## 
      ## $NbClass
      ## [1] 2
      ## 
      ## attr(,"class")
      ## [1] "LiblineaR"

The holdout data can be predicted:

``` r
  predict(svm_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 20.6 
      ## 2 20.8 
      ## 3 21.1 
      ## 4 20.8 
      ## 5 18.9 
      ## 6  6.40
      ## 7  5.90

### Classification Example (`LiblineaR`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  svm_cls_spec <- 
    svm_linear(cost = 1) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("classification") |> 
    set_engine("LiblineaR")
  svm_cls_spec
```

      ## Linear Support Vector Machine Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   cost = 1
      ## 
      ## Computational engine: LiblineaR

Now we create the model fit object:

``` r
  set.seed(1)
  svm_cls_fit <- svm_cls_spec |> fit(Class ~ ., data = data_train)
  svm_cls_fit
```

      ## parsnip model object
      ## 
      ## $TypeDetail
      ## [1] "L2-regularized L2-loss support vector classification dual (L2R_L2LOSS_SVC_DUAL)"
      ## 
      ## $Type
      ## [1] 1
      ## 
      ## $W
      ##              A         B     Bias
      ## [1,] 0.4067922 -1.314783 1.321851
      ## 
      ## $Bias
      ## [1] 1
      ## 
      ## $ClassNames
      ## [1] Class1 Class2
      ## Levels: Class1 Class2
      ## 
      ## $NbClass
      ## [1] 2
      ## 
      ## attr(,"class")
      ## [1] "LiblineaR"

The holdout data can be predicted for hard class predictions.

``` r
  predict(svm_cls_fit, data_test)
```

      ## # A tibble: 10 × 1
      ##    .pred_class
      ##    <fct>      
      ##  1 Class1     
      ##  2 Class1     
      ##  3 Class1     
      ##  4 Class1     
      ##  5 Class2     
      ##  6 Class2     
      ##  7 Class1     
      ##  8 Class1     
      ##  9 Class1     
      ## 10 Class2

With the `"kernlab"` engine

### Regression Example (`kernlab`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  svm_reg_spec <- 
    svm_linear(cost = 1, margin = 0.1) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("regression") |> 
    set_engine("kernlab")
  svm_reg_spec
```

      ## Linear Support Vector Machine Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   cost = 1
      ##   margin = 0.1
      ## 
      ## Computational engine: kernlab

Now we create the model fit object:

``` r
  set.seed(1)
  svm_reg_fit <- svm_reg_spec |> fit(ridership ~ ., data = Chicago_train)
  svm_reg_fit
```

      ## parsnip model object
      ## 
      ## Support Vector Machine object of class "ksvm" 
      ## 
      ## SV type: eps-svr  (regression) 
      ##  parameter : epsilon = 0.1  cost C = 1 
      ## 
      ## Linear (vanilla) kernel function. 
      ## 
      ## Number of Support Vectors : 2283 
      ## 
      ## Objective Function Value : -825.1632 
      ## Training error : 0.226456

The holdout data can be predicted:

``` r
  predict(svm_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 21.0 
      ## 2 21.2 
      ## 3 21.5 
      ## 4 21.2 
      ## 5 19.4 
      ## 6  6.87
      ## 7  6.41

### Classification Example (`kernlab`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  svm_cls_spec <- 
    svm_linear(cost = 1) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("classification") |> 
    set_engine("kernlab")
  svm_cls_spec
```

      ## Linear Support Vector Machine Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   cost = 1
      ## 
      ## Computational engine: kernlab

Now we create the model fit object:

``` r
  set.seed(1)
  svm_cls_fit <- svm_cls_spec |> fit(Class ~ ., data = data_train)
  svm_cls_fit
```

      ## parsnip model object
      ## 
      ## Support Vector Machine object of class "ksvm" 
      ## 
      ## SV type: C-svc  (classification) 
      ##  parameter : cost C = 1 
      ## 
      ## Linear (vanilla) kernel function. 
      ## 
      ## Number of Support Vectors : 353 
      ## 
      ## Objective Function Value : -349.425 
      ## Training error : 0.174136 
      ## Probability model included.

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(svm_cls_fit, data_test),
    predict(svm_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class1             0.517      0.483  
      ##  2 Class1             0.904      0.0956 
      ##  3 Class1             0.645      0.355  
      ##  4 Class1             0.610      0.390  
      ##  5 Class2             0.445      0.555  
      ##  6 Class2             0.212      0.788  
      ##  7 Class1             0.704      0.296  
      ##  8 Class1             0.565      0.435  
      ##  9 Class1             0.994      0.00646
      ## 10 Class2             0.114      0.886

## `svm_poly()` models

With the `"kernlab"` engine

### Regression Example (`kernlab`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(Chicago)
  
  n <- nrow(Chicago)
  Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)
  
  Chicago_train <- Chicago[1:(n - 7), ]
  Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
  svm_reg_spec <- 
    svm_poly(cost = 1, margin = 0.1) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("regression") |> 
    set_engine("kernlab")
  svm_reg_spec
```

      ## Polynomial Support Vector Machine Model Specification (regression)
      ## 
      ## Main Arguments:
      ##   cost = 1
      ##   margin = 0.1
      ## 
      ## Computational engine: kernlab

Now we create the model fit object:

``` r
  set.seed(1)
  svm_reg_fit <- svm_reg_spec |> fit(ridership ~ ., data = Chicago_train)
```

      ##  Setting default kernel parameters

``` r
  svm_reg_fit
```

      ## parsnip model object
      ## 
      ## Support Vector Machine object of class "ksvm" 
      ## 
      ## SV type: eps-svr  (regression) 
      ##  parameter : epsilon = 0.1  cost C = 1 
      ## 
      ## Polynomial kernel function. 
      ##  Hyperparameters : degree =  1  scale =  1  offset =  1 
      ## 
      ## Number of Support Vectors : 2283 
      ## 
      ## Objective Function Value : -825.1628 
      ## Training error : 0.226471

The holdout data can be predicted:

``` r
  predict(svm_reg_fit, Chicago_test)
```

      ## # A tibble: 7 × 1
      ##   .pred
      ##   <dbl>
      ## 1 21.0 
      ## 2 21.2 
      ## 3 21.5 
      ## 4 21.2 
      ## 5 19.4 
      ## 6  6.87
      ## 7  6.41

### Classification Example (`kernlab`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
  library(tidymodels)
  tidymodels_prefer()
  data(two_class_dat)
  
  data_train <- two_class_dat[-(1:10), ]
  data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
  svm_cls_spec <- 
    svm_poly(cost = 1) |> 
    # This model can be used for classification or regression, so set mode
    set_mode("classification") |> 
    set_engine("kernlab")
  svm_cls_spec
```

      ## Polynomial Support Vector Machine Model Specification (classification)
      ## 
      ## Main Arguments:
      ##   cost = 1
      ## 
      ## Computational engine: kernlab

Now we create the model fit object:

``` r
  set.seed(1)
  svm_cls_fit <- svm_cls_spec |> fit(Class ~ ., data = data_train)
```

      ##  Setting default kernel parameters

``` r
  svm_cls_fit
```

      ## parsnip model object
      ## 
      ## Support Vector Machine object of class "ksvm" 
      ## 
      ## SV type: C-svc  (classification) 
      ##  parameter : cost C = 1 
      ## 
      ## Polynomial kernel function. 
      ##  Hyperparameters : degree =  1  scale =  1  offset =  1 
      ## 
      ## Number of Support Vectors : 353 
      ## 
      ## Objective Function Value : -349.425 
      ## Training error : 0.174136 
      ## Probability model included.

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
  bind_cols(
    predict(svm_cls_fit, data_test),
    predict(svm_cls_fit, data_test, type = "prob")
  )
```

      ## # A tibble: 10 × 3
      ##    .pred_class .pred_Class1 .pred_Class2
      ##    <fct>              <dbl>        <dbl>
      ##  1 Class1             0.517      0.483  
      ##  2 Class1             0.904      0.0956 
      ##  3 Class1             0.645      0.355  
      ##  4 Class1             0.610      0.390  
      ##  5 Class2             0.445      0.555  
      ##  6 Class2             0.212      0.788  
      ##  7 Class1             0.704      0.296  
      ##  8 Class1             0.565      0.435  
      ##  9 Class1             0.994      0.00646
      ## 10 Class2             0.114      0.886

## `svm_rbf()` models

With the `"kernlab"` engine

### Regression Example (`kernlab`)

We’ll model the ridership on the Chicago elevated trains as a function
of the 14 day lagged ridership at two stations. The two predictors are
in the same units (rides per day/1000) and do not need to be normalized.
All but the last week of data are used for training. The last week will
be predicted after the model is fit.

``` r
library(tidymodels)
tidymodels_prefer()
data(Chicago)

n <- nrow(Chicago)
Chicago <- Chicago |> select(ridership, Clark_Lake, Quincy_Wells)

Chicago_train <- Chicago[1:(n - 7), ]
Chicago_test <- Chicago[(n - 6):n, ]
```

We can define the model with specific parameters:

``` r
svm_reg_spec <- 
  svm_rbf(cost = 1, margin = 0.1) |> 
  # This model can be used for classification or regression, so set mode
  set_mode("regression") |> 
  set_engine("kernlab")
svm_reg_spec
```

    ## Radial Basis Function Support Vector Machine Model Specification (regression)
    ## 
    ## Main Arguments:
    ##   cost = 1
    ##   margin = 0.1
    ## 
    ## Computational engine: kernlab

Now we create the model fit object:

``` r
set.seed(1)
svm_reg_fit <- svm_reg_spec |> fit(ridership ~ ., data = Chicago_train)
svm_reg_fit
```

    ## parsnip model object
    ## 
    ## Support Vector Machine object of class "ksvm" 
    ## 
    ## SV type: eps-svr  (regression) 
    ##  parameter : epsilon = 0.1  cost C = 1 
    ## 
    ## Gaussian Radial Basis kernel function. 
    ##  Hyperparameter : sigma =  10.8262370251485 
    ## 
    ## Number of Support Vectors : 2233 
    ## 
    ## Objective Function Value : -746.584 
    ## Training error : 0.205567

The holdout data can be predicted:

``` r
predict(svm_reg_fit, Chicago_test)
```

    ## # A tibble: 7 × 1
    ##   .pred
    ##   <dbl>
    ## 1 20.7 
    ## 2 21.2 
    ## 3 21.3 
    ## 4 21.1 
    ## 5 19.4 
    ## 6  6.77
    ## 7  6.13

### Classification Example (`kernlab`)

The example data has two predictors and an outcome with two classes.
Both predictors are in the same units.

``` r
library(tidymodels)
tidymodels_prefer()
data(two_class_dat)

data_train <- two_class_dat[-(1:10), ]
data_test  <- two_class_dat[  1:10 , ]
```

We can define the model with specific parameters:

``` r
svm_cls_spec <- 
  svm_rbf(cost = 1) |> 
  # This model can be used for classification or regression, so set mode
  set_mode("classification") |> 
  set_engine("kernlab")
svm_cls_spec
```

    ## Radial Basis Function Support Vector Machine Model Specification (classification)
    ## 
    ## Main Arguments:
    ##   cost = 1
    ## 
    ## Computational engine: kernlab

Now we create the model fit object:

``` r
set.seed(1)
svm_cls_fit <- svm_cls_spec |> fit(Class ~ ., data = data_train)
svm_cls_fit
```

    ## parsnip model object
    ## 
    ## Support Vector Machine object of class "ksvm" 
    ## 
    ## SV type: C-svc  (classification) 
    ##  parameter : cost C = 1 
    ## 
    ## Gaussian Radial Basis kernel function. 
    ##  Hyperparameter : sigma =  1.63216688499952 
    ## 
    ## Number of Support Vectors : 327 
    ## 
    ## Objective Function Value : -294.4344 
    ## Training error : 0.169014 
    ## Probability model included.

The holdout data can be predicted for both hard class predictions and
probabilities. We’ll bind these together into one tibble:

``` r
bind_cols(
  predict(svm_cls_fit, data_test),
  predict(svm_cls_fit, data_test, type = "prob")
)
```

    ## # A tibble: 10 × 3
    ##    .pred_class .pred_Class1 .pred_Class2
    ##    <fct>              <dbl>        <dbl>
    ##  1 Class2             0.238       0.762 
    ##  2 Class1             0.905       0.0950
    ##  3 Class1             0.619       0.381 
    ##  4 Class1             0.879       0.121 
    ##  5 Class1             0.641       0.359 
    ##  6 Class2             0.153       0.847 
    ##  7 Class1             0.745       0.255 
    ##  8 Class2             0.313       0.687 
    ##  9 Class1             0.878       0.122 
    ## 10 Class2             0.137       0.863
