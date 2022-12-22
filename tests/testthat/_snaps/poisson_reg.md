# updating

    Code
      poisson_reg(penalty = 1) %>% set_engine("glmnet", lambda.min.ratio = 0.001) %>%
        update(mixture = tune())
    Message
      ! parsnip could not locate an implementation for `poisson_reg` model specifications using the `glmnet` engine.
      i The parsnip extension package poissonreg implements support for this specification.
      i Please install (if needed) and load to continue.
    Output
      Poisson Regression Model Specification (regression)
      
      Main Arguments:
        penalty = 1
        mixture = tune()
      
      Engine-Specific Arguments:
        lambda.min.ratio = 0.001
      
      Computational engine: glmnet 
      

# bad input

    Code
      poisson_reg(mode = "bogus")
    Condition
      Error in `poisson_reg()`:
      ! 'bogus' is not a known mode for model `poisson_reg()`.

---

    Code
      translate(poisson_reg(mode = "regression"), engine = NULL)
    Condition
      Error in `translate.default()`:
      ! Please set an engine.

---

    Code
      poisson_reg(formula = y ~ x)
    Condition
      Error in `poisson_reg()`:
      ! unused argument (formula = y ~ x)

