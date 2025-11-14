# Data Set Characteristics Available when Fitting Models

When using the [`fit()`](https://generics.r-lib.org/reference/fit.html)
functions there are some variables that will be available for use in
arguments. For example, if the user would like to choose an argument
value based on the current number of rows in a data set, the `.obs()`
function can be used. See Details below.

## Usage

``` r
.cols()

.preds()

.obs()

.lvls()

.facts()

.x()

.y()

.dat()
```

## Details

Existing functions:

- `.obs()`: The current number of rows in the data set.

- `.preds()`: The number of columns in the data set that is associated
  with the predictors prior to dummy variable creation.

- `.cols()`: The number of predictor columns available after dummy
  variables are created (if any).

- `.facts()`: The number of factor predictors in the data set.

- `.lvls()`: If the outcome is a factor, this is a table with the counts
  for each level (and `NA` otherwise).

- `.x()`: The predictors returned in the format given. Either a data
  frame or a matrix.

- `.y()`: The known outcomes returned in the format given. Either a
  vector, matrix, or data frame.

- `.dat()`: A data frame containing all of the predictors and the
  outcomes. If
  [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) was
  used, the outcomes are attached as the column, `..y`.

For example, if you use the model formula `circumference ~ .` with the
built-in `Orange` data, the values would be

     .preds() =   2          (the 2 remaining columns in `Orange`)
     .cols()  =   5          (1 numeric column + 4 from Tree dummy variables)
     .obs()   = 35
     .lvls()  =  NA          (no factor outcome)
     .facts() =   1          (the Tree predictor)
     .y()     = <vector>     (circumference as a vector)
     .x()     = <data.frame> (The other 2 columns as a data frame)
     .dat()   = <data.frame> (The full data set)

If the formula `Tree ~ .` were used:

     .preds() =   2          (the 2 numeric columns in `Orange`)
     .cols()  =   2          (same)
     .obs()   = 35
     .lvls()  =  c("1" = 7, "2" = 7, "3" = 7, "4" = 7, "5" = 7)
     .facts() =   0
     .y()     = <vector>     (Tree as a vector)
     .x()     = <data.frame> (The other 2 columns as a data frame)
     .dat()   = <data.frame> (The full data set)

To use these in a model fit, pass them to a model specification. The
evaluation is delayed until the time when the model is run via
[`fit()`](https://generics.r-lib.org/reference/fit.html) (and the
variables listed above are available). For example:


    library(modeldata)
    data("lending_club")

    rand_forest(mode = "classification", mtry = .cols() - 2)

When no descriptors are found, the computation of the descriptor values
is not executed.
