# updating

    Code
      decision_tree(cost_complexity = 0.1) %>% set_engine("rpart", model = FALSE) %>%
        update(cost_complexity = tune(), model = tune())
    Output
      Decision Tree Model Specification (unknown)
      
      Main Arguments:
        cost_complexity = tune()
      
      Engine-Specific Arguments:
        model = tune()
      
      Computational engine: rpart 
      

# bad input

    'bogus' is not a known mode for model `decision_tree()`.

---

    Please set the mode in the model specification.

---

    Please set the mode in the model specification.

---

    Model code depends on the mode; please specify one.

---

    unused argument (formula = y ~ x)

