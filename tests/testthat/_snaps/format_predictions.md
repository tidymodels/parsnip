# format_num() is deprecated

    Code
      . <- format_num(c(1, 2, 3))
    Condition
      Warning:
      `format_num()` was deprecated in parsnip 1.5.0.
      i Use `format_predictions(x, "numeric")` instead.

# format_class() is deprecated

    Code
      . <- format_class(factor(c("a", "b")))
    Condition
      Warning:
      `format_class()` was deprecated in parsnip 1.5.0.
      i Use `format_predictions(x, "class")` instead.

# format_classprobs() is deprecated

    Code
      . <- format_classprobs(data.frame(a = 0.5, b = 0.5))
    Condition
      Warning:
      `format_classprobs()` was deprecated in parsnip 1.5.0.
      i Use `format_predictions(x, "prob")` instead.

# format_time() is deprecated

    Code
      . <- format_time(c(1, 2, 3))
    Condition
      Warning:
      `format_time()` was deprecated in parsnip 1.5.0.
      i Use `format_predictions(x, "time")` instead.

# format_survival() is deprecated

    Code
      . <- format_survival(c(0.9, 0.8))
    Condition
      Warning:
      `format_survival()` was deprecated in parsnip 1.5.0.
      i Use `format_predictions(x, "survival")` instead.

# format_linear_pred() is deprecated

    Code
      . <- format_linear_pred(c(-1, 0, 1))
    Condition
      Warning:
      `format_linear_pred()` was deprecated in parsnip 1.5.0.
      i Use `format_predictions(x, "linear_pred")` instead.

# format_hazard() is deprecated

    Code
      . <- format_hazard(c(0.1, 0.2))
    Condition
      Warning:
      `format_hazard()` was deprecated in parsnip 1.5.0.
      i Use `format_predictions(x, "hazard")` instead.

