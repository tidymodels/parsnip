# predict(type = "prob") with level "class" (see #720)

    Code
      predict(mod, type = "prob", new_data = x)
    Condition
      Error in `check_spec_levels()`:
      ! The outcome variable `boop` has a level called "class".
      i This value is reserved for parsnip's classification internals; please change the levels, perhaps with `forcats::fct_relevel()`.

# non-factor classification

    Code
      fit(set_engine(logistic_reg(), "glm"), class ~ ., data = dplyr::mutate(hpc,
        class = class == "VF"))
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a <factor>, not a logical vector.

---

    Code
      fit(set_engine(logistic_reg(), "glm"), class ~ ., data = dplyr::mutate(hpc,
        class = ifelse(class == "VF", 1, 0)))
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a <factor>, not a double vector.

---

    Code
      fit(set_engine(multinom_reg(), "glmnet"), class ~ ., data = dplyr::mutate(hpc,
        class = as.character(class)))
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a <factor>, not a character vector.

