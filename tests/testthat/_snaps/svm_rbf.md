# engine arguments

    Code
      translate(kernlab_cv, "kernlab")$method$fit$args
    Output
      $x
      missing_arg()
      
      $data
      missing_arg()
      
      $cross
      <quosure>
      expr: ^10
      env:  empty
      
      $kernel
      [1] "rbfdot"
      

# updating

    Code
      update(set_engine(svm_rbf(mode = "regression", rbf_sigma = 0.3), "kernlab",
      cross = 10), rbf_sigma = tune(), cross = tune())
    Output
      Radial Basis Function Support Vector Machine Model Specification (regression)
      
      Main Arguments:
        rbf_sigma = tune()
      
      Engine-Specific Arguments:
        cross = tune()
      
      Computational engine: kernlab 
      

# bad input

    Code
      svm_rbf(mode = "reallyunknown")
    Condition
      Error in `svm_rbf()`:
      ! "reallyunknown" is not a known mode for model `svm_rbf()`.

---

    Code
      translate(set_engine(svm_rbf(mode = "regression"), NULL))
    Condition
      Error in `set_engine()`:
      ! Missing engine. Possible mode/engine combinations are: classification {kernlab, liquidSVM} and regression {kernlab, liquidSVM}.

