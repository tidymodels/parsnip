# updating

    Code
      update(set_engine(decision_tree(cost_complexity = 0.1), "rpart", model = FALSE),
      cost_complexity = tune(), model = tune())
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

    Please set the mode in the model specification (`?parsnip::model_spec()`).

---

    Please set the mode in the model specification (`?parsnip::model_spec()`).

---

    Code
      try(translate(decision_tree(), engine = NULL), silent = TRUE)
    Message
      Used `engine = 'rpart'` for translation.

# argument checks for data dimensions

    Code
      f_fit <- fit(spec, body_mass_g ~ ., data = penguins)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 samples will be used.

---

    Code
      xy_fit <- fit_xy(spec, x = penguins[, -6], y = penguins$body_mass_g)
    Condition
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 samples will be used.

