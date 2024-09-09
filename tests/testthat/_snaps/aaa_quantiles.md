# vec_quantiles error types

    Code
      vec_quantiles(1:10, 1:4 / 5)
    Condition
      Error in `vec_quantiles()`:
      ! `values` must be a <matrix>, not an integer vector.

---

    Code
      vec_quantiles(matrix(1:20, 5), -1:4 / 5)
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `vec_quantiles()`:
      ! `quantile_levels` must be a number between 0 and 1, not the number -0.2.

---

    Code
      vec_quantiles(matrix(1:20, 5), 1:5 / 6)
    Condition
      Error in `vec_quantiles()`:
      ! The number of columns in `values` must be equal to the length of `quantile_levels`.

---

    Code
      vec_quantiles(matrix(1:20, 5), 4:1 / 5)
    Condition
      Error in `vec_quantiles()`:
      ! `quantile_levels` must be sorted in increasing order.

