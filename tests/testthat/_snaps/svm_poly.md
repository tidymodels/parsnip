# updating

    Code
      update(set_engine(svm_poly(mode = "regression", degree = 2), "kernlab", cross = 10),
      degree = tune(), cross = tune())
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
      translate(set_engine(svm_poly(), NULL))
    Condition
      Error in `set_engine()`:
      ! Missing engine. Possible mode/engine combinations are: classification {kernlab} and regression {kernlab}.

