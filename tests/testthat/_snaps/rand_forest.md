# updating

    Code
      update(set_engine(rand_forest(mode = "regression", mtry = 2), "randomForest",
      sampsize = 10), mtry = tune(), sampsize = tune())
    Output
      Random Forest Model Specification (regression)
      
      Main Arguments:
        mtry = tune()
      
      Engine-Specific Arguments:
        sampsize = tune()
      
      Computational engine: randomForest 
      

# bad input

    Code
      res <- translate(set_engine(rand_forest(mode = "classification"), NULL))
    Condition
      Error in `set_engine()`:
      ! Missing engine. Possible mode/engine combinations are: classification {ranger, randomForest, spark} and regression {ranger, randomForest, spark}.

---

    Code
      rand_forest(mode = "time series")
    Condition
      Error in `rand_forest()`:
      ! "time series" is not a known mode for model `rand_forest()`.

---

    Code
      translate(set_engine(rand_forest(mode = "classification"), "wat?"))
    Condition
      Error in `set_engine()`:
      x Engine "wat?" is not supported for `rand_forest()`
      i See `show_engines("rand_forest")`.

