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

# argument checks for data dimensions

    Code
      f_fit <- spec %>% fit(body_mass_g ~ ., data = penguins)
    Condition
      Warning:
      1000 columns were requested but there were 6 predictors in the data. 6 will be used.
      Warning:
      1000 samples were requested but there were 333 rows in the data. 333 will be used.

---

    Code
      xy_fit <- spec %>% fit_xy(x = penguins[, -6], y = penguins$body_mass_g)
    Condition
      Warning:
      1000 columns were requested but there were 6 predictors in the data. 6 will be used.
      Warning:
      1000 samples were requested but there were 333 rows in the data. 333 will be used.

