# updating

    Code
      svm_poly(mode = "regression", degree = 2) %>% set_engine("kernlab", cross = 10) %>%
        update(degree = tune(), cross = tune())
    Output
      Polynomial Support Vector Machine Model Specification (regression)
      
      Main Arguments:
        degree = tune()
      
      Engine-Specific Arguments:
        cross = tune()
      
      Computational engine: kernlab 
      

# bad input

    Code
      svm_poly(mode = "reallyunknown")
    Condition
      Error in `svm_poly()`:
      ! "reallyunknown" is not a known mode for model `svm_poly()`.

---

    Code
      svm_poly() %>% set_engine(NULL) %>% translate()
    Condition
      Error in `translate.default()`:
      ! Please set an engine.

