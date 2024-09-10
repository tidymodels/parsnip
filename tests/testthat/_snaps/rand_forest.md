# updating

    Code
      rand_forest(mode = "regression", mtry = 2) %>% set_engine("randomForest",
        sampsize = 10) %>% update(mtry = tune(), sampsize = tune())
    Output
      Random Forest Model Specification (regression)
      
      Main Arguments:
        mtry = tune()
      
      Engine-Specific Arguments:
        sampsize = tune()
      
      Computational engine: randomForest 
      

# bad input

    Code
      translate(rand_forest(mode = "classification") %>% set_engine(NULL))
    Condition
      Error in `if (object$engine == "liquidSVM") ...`:
      ! argument is of length zero

---

    Code
      rand_forest(mode = "time series")
    Condition
      Error in `rand_forest()`:
      ! "time series" is not a known mode for model `rand_forest()`.

---

    Code
      translate(rand_forest(mode = "classification") %>% set_engine("wat?"))
    Condition
      Error in `set_engine()`:
      x Engine "wat?" is not supported for `rand_forest()`
      i See `show_engines("rand_forest")`.

---

    Code
      translate(rand_forest(mode = "classification", ytest = 2))
    Condition
      Error in `rand_forest()`:
      ! unused argument (ytest = 2)

