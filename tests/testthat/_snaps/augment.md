# regression models

    Code
      augment(reg_form, head(mtcars[, -1]))
    Condition
      Error in `augment()`:
      ! Unknown mode "depeche".
      i Model mode should be one of "classification", "regression", "censored regression", or "quantile regression".

# quantile regression models

    Code
      linear_reg() %>% set_mode("quantile regression", quantile_levels = probs_1)
    Output
      Linear Regression Model Specification (quantile regression)
      
      Computational engine: lm 
      
    Message
      Quantile levels: 0.2, 0.4, 0.6, 0.8, and 1.

