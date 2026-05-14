# `predict_survival()` errors early when `add_censoring_weights = TRUE` but `new_data` has no Surv column

    Code
      predict_survival(fake_fit, new_data = no_surv_data, eval_time = 1,
        add_censoring_weights = TRUE)
    Condition
      Error in `predict_survival()`:
      ! `add_censoring_weights = TRUE` requires the survival outcome in `new_data`.
      i Add a <Surv> column to `new_data`.

# .get_surv() errors when y_var names a column that is not a Surv

    Code
      .get_surv(fake_fit, new_data)
    Condition
      Error:
      ! `add_censoring_weights = TRUE` requires the survival outcome in `new_data`.
      i Add a <Surv> column, or include the variable used in the model formula ("outcome").

# .get_surv() errors when the y_var column is missing from new_data

    Code
      .get_surv(fake_fit, new_data)
    Condition
      Error:
      ! `add_censoring_weights = TRUE` requires the survival outcome in `new_data`.
      i Add a <Surv> column, or include the variable used in the model formula ("outcome").

# .get_surv() errors when y_var has two names but columns are missing

    Code
      .get_surv(fake_fit, new_data)
    Condition
      Error:
      ! `add_censoring_weights = TRUE` requires the survival outcome in `new_data`.
      i Add a <Surv> column, or include the variables used in the model formula ("time" and "status").

# .get_surv() errors when y_var has more than two names

    Code
      .get_surv(fake_fit, new_data)
    Condition
      Error:
      ! `add_censoring_weights = TRUE` requires the survival outcome in `new_data`.
      i Add a <Surv> column, or include the variables used in the model formula ("a", "b", and "c").

