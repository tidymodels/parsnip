# Obtain names of prediction columns for a fitted model or workflow

`.get_prediction_column_names()` returns a list that has the names of
the columns for the primary prediction types for a model.

## Usage

``` r
.get_prediction_column_names(x, syms = FALSE)
```

## Arguments

- x:

  A fitted parsnip model (class `"model_fit"`) or a fitted workflow.

- syms:

  Should the column names be converted to symbols? Defaults to `FALSE`.

## Value

A list with elements `"estimate"` and `"probabilities"`.

## Examples

``` r
library(dplyr)
library(modeldata)
#> 
#> Attaching package: ‘modeldata’
#> The following object is masked from ‘package:datasets’:
#> 
#>     penguins
data("two_class_dat")

levels(two_class_dat$Class)
#> [1] "Class1" "Class2"
lr_fit <- logistic_reg() |> fit(Class ~ ., data = two_class_dat)

.get_prediction_column_names(lr_fit)
#> $estimate
#> [1] ".pred_class"
#> 
#> $probabilities
#> [1] ".pred_Class1" ".pred_Class2"
#> 
.get_prediction_column_names(lr_fit, syms = TRUE)
#> $estimate
#> $estimate[[1]]
#> .pred_class
#> 
#> 
#> $probabilities
#> $probabilities[[1]]
#> .pred_Class1
#> 
#> $probabilities[[2]]
#> .pred_Class2
#> 
#> 
```
