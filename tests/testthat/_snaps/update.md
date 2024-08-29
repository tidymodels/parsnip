# update methods work (eg: linear_reg)

    Code
      expr1 %>% update(mixture = 0)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        mixture = 0
      
      Engine-Specific Arguments:
        model = FALSE
      
      Computational engine: lm 
      

---

    Code
      expr1 %>% update(mixture = 0, fresh = TRUE)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        mixture = 0
      
      Computational engine: lm 
      

---

    Code
      expr2 %>% update(nlambda = 10)
    Output
      Linear Regression Model Specification (regression)
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      expr3 %>% update(mixture = 1, nlambda = 10)
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
      expr3 %>% update(mixture = 1, nlambda = 10, fresh = TRUE)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        mixture = 1
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      expr3 %>% update(nlambda = 10)
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
      expr3 %>% update(nlambda = 10, fresh = TRUE)
    Output
      Linear Regression Model Specification (regression)
      
      Engine-Specific Arguments:
        nlambda = 10
      
      Computational engine: glmnet 
      

---

    Code
      expr4 %>% update(param_tibb)
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
      expr4 %>% update(param_list)
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
      expr4 %>% update(param_tibb, fresh = TRUE)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        penalty = 1
        mixture = 0.5
      
      Computational engine: glmnet 
      

---

    Code
      expr4 %>% update(param_list, fresh = TRUE)
    Output
      Linear Regression Model Specification (regression)
      
      Main Arguments:
        penalty = 1
        mixture = 0.5
      
      Computational engine: glmnet 
      

---

    Code
      expr5 %>% update(family = "poisson")
    Output
      Linear Regression Model Specification (regression)
      
      Engine-Specific Arguments:
        family = poisson
      
      Computational engine: glm 
      

---

    Code
      expr5 %>% update(family = "poisson", fresh = TRUE)
    Output
      Linear Regression Model Specification (regression)
      
      Engine-Specific Arguments:
        family = poisson
      
      Computational engine: glm 
      

# update methods prompt informatively

    Code
      expr1 %>% update(param_tibb)
    Condition
      Error in `update_main_parameters()`:
      ! Argument `nlambda` is not a main argument.

---

    Code
      expr1 %>% update(param_list)
    Condition
      Error in `update_main_parameters()`:
      ! Argument `nlambda` is not a main argument.

---

    Code
      expr1 %>% update(parameters = "wat")
    Condition
      Error in `check_final_param()`:
      ! The parameter object should be a list or tibble.

---

    Code
      expr1 %>% update(parameters = tibble::tibble(wat = "wat"))
    Condition
      Error in `update_main_parameters()`:
      ! Argument `wat` is not a main argument.

---

    Code
      linear_reg() %>% update(boop = 0)
    Condition
      Error in `update_dot_check()`:
      ! The extra argument `boop` will be ignored.

