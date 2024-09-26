# ranger classification execution

    Code
      res <- fit(lc_ranger, funded_amnt ~ Class + term, data = lending_club, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a `factor`, not a `integer`.

---

    Code
      res <- fit(bad_ranger_cls, funded_amnt ~ term, data = lending_club, control = ctrl)
    Condition
      Error in `check_outcome()`:
      ! For a classification model, the outcome should be a `factor`, not a `integer`.

# ranger classification probabilities

    Code
      parsnip:::predict_classprob.model_fit(no_prob_model, new_data = lending_club[1:
      6, num_pred])
    Condition
      Error in `predict()`:
      ! `ranger` model does not appear to use class probabilities.
      i Was the model fit with `probability = TRUE`?

# ranger regression intervals

    Code
      rgr_se <- predict(extract_fit_engine(xy_fit), data = head(ames_x, 3), type = "se")$
        se
    Condition
      Warning in `rInfJack()`:
      Sample size <=20, no calibration performed.
      Warning in `sqrt()`:
      NaNs produced

---

    Code
      parsnip_int <- predict(xy_fit, new_data = head(ames_x, 3), type = "conf_int",
      std_error = TRUE, level = 0.93)
    Condition
      Warning in `rInfJack()`:
      Sample size <=20, no calibration performed.
      Warning in `sqrt()`:
      NaNs produced

# ranger classification intervals

    Code
      rgr_se <- predict(extract_fit_engine(lc_fit), data = tail(lending_club), type = "se")$
        se
    Condition
      Warning in `rInfJack()`:
      Sample size <=20, no calibration performed.
      Warning in `rInfJack()`:
      Sample size <=20, no calibration performed.
      Warning in `sqrt()`:
      NaNs produced

---

    Code
      parsnip_int <- predict(lc_fit, new_data = tail(lending_club), type = "conf_int",
      std_error = TRUE, level = 0.93)
    Condition
      Warning in `rInfJack()`:
      Sample size <=20, no calibration performed.
      Warning in `rInfJack()`:
      Sample size <=20, no calibration performed.
      Warning in `sqrt()`:
      NaNs produced

# argument checks for data dimensions

    Code
      f_fit <- spec %>% fit(body_mass_g ~ ., data = penguins)
    Condition
      Warning:
      ! 1000 columns were requested but there were 6 predictors in the data.
      i 6 predictors will be used.
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 samples will be used.

---

    Code
      xy_fit <- spec %>% fit_xy(x = penguins[, -6], y = penguins$body_mass_g)
    Condition
      Warning:
      ! 1000 columns were requested but there were 6 predictors in the data.
      i 6 predictors will be used.
      Warning:
      ! 1000 samples were requested but there were 333 rows in the data.
      i 333 samples will be used.

