# classification requires exactly two outcome levels

    Code
      fit(spec, Species ~ ., data = unused_last)
    Condition
      Error in `fit()`:
      ! BART classification with the "dbarts" engine requires an outcome with exactly 2 levels, but 3 were given: "setosa", "versicolor", and "virginica".
      i dbarts models a binary outcome, so additional levels cannot be fit.
      i Only "setosa" and "versicolor" appear in the data.
      i Use `droplevels()` to drop the unused level "virginica".

---

    Code
      fit(spec, Species ~ ., data = unused_first)
    Condition
      Error in `fit()`:
      ! BART classification with the "dbarts" engine requires an outcome with exactly 2 levels, but 3 were given: "setosa", "versicolor", and "virginica".
      i dbarts models a binary outcome, so additional levels cannot be fit.
      i Only "versicolor" and "virginica" appear in the data.
      i Use `droplevels()` to drop the unused level "setosa".

---

    Code
      fit(spec, Species ~ ., data = iris)
    Condition
      Error in `fit()`:
      ! BART classification with the "dbarts" engine requires an outcome with exactly 2 levels, but 3 were given: "setosa", "versicolor", and "virginica".
      i dbarts models a binary outcome, so additional levels cannot be fit.

