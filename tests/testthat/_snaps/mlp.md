# updating

    Code
      mlp(mode = "classification", hidden_units = 2) %>% set_engine("nnet", Hess = FALSE) %>%
        update(hidden_units = tune(), Hess = tune())
    Output
      Single Layer Neural Network Model Specification (classification)
      
      Main Arguments:
        hidden_units = tune()
      
      Engine-Specific Arguments:
        Hess = tune()
      
      Computational engine: nnet 
      

# bad input

    Code
      mlp(mode = "time series")
    Condition
      Error in `mlp()`:
      ! "time series" is not a known mode for model `mlp()`.

---

    Code
      translate(mlp(mode = "classification") %>% set_engine("wat?"))
    Condition
      Error in `set_engine()`:
      x Engine "wat?" is not supported for `mlp()`
      i See `show_engines("mlp")`.

---

    Code
      translate(mlp(mode = "classification", x = x, y = y) %>% set_engine("keras"))
    Condition
      Error in `mlp()`:
      ! unused arguments (x = x, y = y)

---

    Code
      translate(mlp(mode = "regression", formula = y ~ x) %>% set_engine())
    Condition
      Error in `mlp()`:
      ! unused argument (formula = y ~ x)

# check_args() works

    Code
      spec <- mlp(penalty = -1) %>% set_engine("keras") %>% set_mode("classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      spec <- mlp(dropout = -1) %>% set_engine("keras") %>% set_mode("classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `dropout` must be a number between 0 and 1 or `NULL`, not the number -1.

---

    Code
      spec <- mlp(dropout = 1, penalty = 3) %>% set_engine("keras") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! Both weight decay and dropout should not be specified.

