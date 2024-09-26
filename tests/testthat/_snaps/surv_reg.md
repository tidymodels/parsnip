# updating

    Code
      surv_reg() %>% set_engine("flexsurv", cl = 0.99) %>% update(cl = tune())
    Output
      Parametric Survival Regression Model Specification (regression)
      
      Engine-Specific Arguments:
        cl = tune()
      
      Computational engine: flexsurv 
      

# bad input

    Code
      surv_reg(mode = ", classification")
    Condition
      Error in `surv_reg()`:
      ! ", classification" is not a known mode for model `surv_reg()`.

---

    Code
      translate(surv_reg() %>% set_engine("wat"))
    Condition
      Error in `set_engine()`:
      x Engine "wat" is not supported for `surv_reg()`
      i See `show_engines("surv_reg")`.

---

    Code
      res <- translate(surv_reg() %>% set_engine(NULL))
    Message
      Used `engine = 'survival'` for translation.

# deprecation warning

    Code
      surv_reg()
    Condition
      Warning:
      `surv_reg()` was deprecated in parsnip 0.1.6.
      i Please use `survival_reg()` instead.
    Output
      Parametric Survival Regression Model Specification (regression)
      
      Computational engine: survival 
      

