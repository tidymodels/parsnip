# predict(type = "prob") with level "class" (see #720)

    Code
      predict(mod, type = "prob", new_data = x)
    Condition
      Error in `check_spec_levels()`:
      ! The outcome variable `boop` has a level called "class".
      i This value is reserved for parsnip's classification internals; please change the levels, perhaps with `forcats::fct_relevel()`.

# non-factor classification

    Code
      logistic_reg() %>% set_engine("glm") %>% fit(class ~ ., data = hpc %>% dplyr::mutate(
        class = class == "VF"))
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a `factor`, not a `logical`.

---

    Code
      logistic_reg() %>% set_engine("glm") %>% fit(class ~ ., data = hpc %>% dplyr::mutate(
        class = ifelse(class == "VF", 1, 0)))
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a `factor`, not a `numeric`.

---

    Code
      multinom_reg() %>% set_engine("glmnet") %>% fit(class ~ ., data = hpc %>%
        dplyr::mutate(class = as.character(class)))
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a `factor`, not a `character`.

