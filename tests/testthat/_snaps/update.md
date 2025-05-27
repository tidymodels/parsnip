# update methods work (eg: linear_reg)

    Code
      update(expr1, mixture = 0)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        mixture = 0
      
      Engine-Specific Arguments:
        model = FALSE
      
      Computational engine: lm 
      

---

    Code
      update(expr1, mixture = 0, fresh = TRUE)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        mixture = 0
      
      Computational engine: lm 
      

---

    Code
      update(expr2, nlambda = 10)
    Output
      Linear Regression Model Specification (regression)
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      update(expr3, mixture = 1, nlambda = 10)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        penalty = tune()
        mixture = 1
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      update(expr3, mixture = 1, nlambda = 10, fresh = TRUE)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        mixture = 1
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      update(expr3, nlambda = 10)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        penalty = tune()
        mixture = 0
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      update(expr3, nlambda = 10, fresh = TRUE)
    Output
      Linear Regression Model Specification (regression)
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      update(expr4, param_tibb)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        penalty = 1
        mixture = 0.5
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      update(expr4, param_list)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        penalty = 1
        mixture = 0.5
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      update(expr4, param_tibb, fresh = TRUE)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        penalty = 1
        mixture = 0.5
      
      Computational engine: glmnet 
      

---

    Code
      update(expr4, param_list, fresh = TRUE)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        penalty = 1
        mixture = 0.5
      
      Computational engine: glmnet 
      

---

    Code
      update(expr5, family = "poisson")
    Output
      Linear Regression Model Specification (regression)
      
      Engine-Specific Arguments:
        family = poisson
      
      Computational engine: glm 
      

---

    Code
      update(expr5, family = "poisson", fresh = TRUE)
    Output
      Linear Regression Model Specification (regression)
      
      Engine-Specific Arguments:
        family = poisson
      
      Computational engine: glm 
      

# update methods prompt informatively

    Code
      update(expr1, param_tibb)
    Condition
      Error in `update()`:
      ! Argument `nlambda` is not a main argument.

---

    Code
      update(expr1, param_list)
    Condition
      Error in `update()`:
      ! Argument `nlambda` is not a main argument.

---

    Code
      update(expr1, parameters = "wat")
    Condition
      Error in `update()`:
      ! The parameter object should be a list or tibble.

---

    Code
      update(expr1, parameters = tibble::tibble(wat = "wat"))
    Condition
      Error in `update()`:
      ! Argument `wat` is not a main argument.

---

    Code
      update(linear_reg(), boop = 0)
    Condition
      Error in `update_dot_check()`:
      ! The extra argument `boop` will be ignored.

