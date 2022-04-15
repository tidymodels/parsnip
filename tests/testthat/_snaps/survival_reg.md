# primary argument

    Code
      normal$args
    Output
      $dist
      <quosure>
      expr: ^"lnorm"
      env:  empty
      

---

    Code
      dist_v$args
    Output
      $dist
      <quosure>
      expr: ^tune()
      env:  0x7fac639ef120
      

# updating

    Code
      basic %>% update(dist = "lnorm")
    Output
      Parametric Survival Regression Model Specification (censored regression)
      
      Main Arguments:
        dist = lnorm
      
      Computational engine: survival 
      

---

    Code
      basic %>% update(param_tibb)
    Output
      Parametric Survival Regression Model Specification (censored regression)
      
      Main Arguments:
        dist = weibull
      
      Computational engine: survival 
      

---

    Code
      basic %>% update(param_list)
    Output
      Parametric Survival Regression Model Specification (censored regression)
      
      Main Arguments:
        dist = weibull
      
      Computational engine: survival 
      

