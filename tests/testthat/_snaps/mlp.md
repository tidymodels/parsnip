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
      

# check_args() works

    Code
      spec <- mlp(penalty = -1) %>% set_engine("nnet") %>% set_mode("classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `penalty` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      spec <- mlp(dropout = -1) %>% set_engine("nnet") %>% set_mode("classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! `dropout` must be a number between 0 and 1 or `NULL`, not the number -1.

---

    Code
      spec <- mlp(dropout = 1, penalty = 3) %>% set_engine("nnet") %>% set_mode(
        "classification")
      fit(spec, class ~ ., hpc)
    Condition
      Error in `fit()`:
      ! Both weight decay and dropout should not be specified.

