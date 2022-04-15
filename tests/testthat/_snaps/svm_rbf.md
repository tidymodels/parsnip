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
      expr1 %>% update(rbf_sigma = 0.1)
    Output
      Radial Basis Function Support Vector Machine Specification (regression)
      
      Main Arguments:
        rbf_sigma = 0.1
      
      Engine-Specific Arguments:
        cross = 10
      
      Computational engine: kernlab 
      

---

    Code
      expr1 %>% update(param_tibb)
    Output
      Radial Basis Function Support Vector Machine Specification (regression)
      
      Main Arguments:
        cost = 10
        rbf_sigma = 3
      
      Engine-Specific Arguments:
        cross = 10
      
      Computational engine: kernlab 
      

---

    Code
      expr1 %>% update(param_list)
    Output
      Radial Basis Function Support Vector Machine Specification (regression)
      
      Main Arguments:
        cost = 10
        rbf_sigma = 3
      
      Engine-Specific Arguments:
        cross = 10
      
      Computational engine: kernlab 
      

---

    Code
      expr2 %>% update(cross = 10)
    Output
      Radial Basis Function Support Vector Machine Specification (regression)
      
      Engine-Specific Arguments:
        cross = 10
      
      Computational engine: kernlab 
      

---

    Code
      expr3 %>% update(rbf_sigma = 0.3, fresh = TRUE)
    Output
      Radial Basis Function Support Vector Machine Specification (regression)
      
      Main Arguments:
        rbf_sigma = 0.3
      
      Computational engine: kernlab 
      

