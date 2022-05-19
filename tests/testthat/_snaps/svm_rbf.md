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
      svm_rbf(mode = "regression", rbf_sigma = 0.3) %>% set_engine("kernlab", cross = 10) %>%
        update(rbf_sigma = tune(), cross = tune())
    Output
      Radial Basis Function Support Vector Machine Specification (regression)
      
      Main Arguments:
        rbf_sigma = tune()
      
      Engine-Specific Arguments:
        cross = tune()
      
      Computational engine: kernlab 
      

