# convert to matrix

    Code
      parsnip::maybe_matrix(ames[, c("Year_Built", "Neighborhood")])
    Condition
      Error in `parsnip::maybe_matrix()`:
      ! The column "Neighborhood" is non-numeric, so the data cannot be converted to a numeric matrix.

---

    Code
      parsnip::maybe_matrix(Chicago[, c("ridership", "date")])
    Condition
      Error in `parsnip::maybe_matrix()`:
      ! The column "date" is non-numeric, so the data cannot be converted to a numeric matrix.

