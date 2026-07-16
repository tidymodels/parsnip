# tabular_pfn() accepts its modes

    Code
      tabular_pfn(mode = "quantile regression")
    Condition
      Error in `tabular_pfn()`:
      ! "quantile regression" is not a known mode for model `tabular_pfn()`.

# check_args.tabular_pfn() validates input values

    Code
      check_args(tabular_pfn(mode = "classification", softmax_temperature = -1))
    Condition
      Error:
      ! `softmax_temperature` must be a number larger than or equal to 0 or `NULL`, not the number -1.

---

    Code
      check_args(tabular_pfn(mode = "classification", num_estimators = 5.5))
    Condition
      Error:
      ! `num_estimators` must be a whole number or `NULL`, not the number 5.5.

---

    Code
      check_args(tabular_pfn(mode = "classification", balance_probabilities = "yes"))
    Condition
      Error:
      ! `balance_probabilities` must be a logical vector or `NULL`, not the string "yes".

---

    Code
      check_args(tabular_pfn(mode = "classification", average_before_softmax = 1))
    Condition
      Error:
      ! `average_before_softmax` must be a logical vector or `NULL`, not the number 1.

