# updating

    Code
      decision_tree(cost_complexity = 0.1) %>% set_engine("rpart", model = FALSE) %>%
        update(cost_complexity = tune(), model = tune())
    Output
      Decision Tree Model Specification (unknown mode)
      
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

    Code
      try(translate(decision_tree(), engine = NULL), silent = TRUE)
    Message
      Used `engine = 'rpart'` for translation.

---

    unused argument (formula = y ~ x)

