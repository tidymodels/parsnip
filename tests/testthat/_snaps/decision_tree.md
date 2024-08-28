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

    "bogus" is not a known mode for model `decision_tree()`.

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

# argument checks for data dimensions

    Code
      f_fit <- spec %>% fit(body_mass_g ~ ., data = penguins)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 will be used.

---

    Code
      xy_fit <- spec %>% fit_xy(x = penguins[, -6], y = penguins$body_mass_g)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 will be used.

