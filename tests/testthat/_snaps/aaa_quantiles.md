# quantile_pred error types

    Code
      quantile_pred(1:10, 1:4 / 5)
    Condition
      Error in `quantile_pred()`:
      ! `values` must be a <matrix>, not an integer vector.

---

    Code
      quantile_pred(matrix(1:20, 5), -1:4 / 5)
    Condition
      Error in `quantile_pred()`:
      ! `quantile_levels` must be a number between 0 and 1, not the number -0.2.

---

    Code
      quantile_pred(matrix(1:20, 5), 1:5 / 6)
    Condition
      Error in `quantile_pred()`:
      ! The number of columns in `values` must be equal to the length of `quantile_levels`.

---

    Code
      quantile_pred(matrix(1:20, 5), 4:1 / 5)
    Condition
      Error in `quantile_pred()`:
      ! `quantile_levels` must be sorted in increasing order.

# quantile_pred formatting

    Code
      v
    Output
      <quantiles[5]>
      [1] [8.5]  [9.5]  [10.5] [11.5] [12.5]
      # Quantile levels:  0.2 0.4 0.6 0.8 

---

    Code
      quantile_pred(matrix(1:18, 9), c(1 / 3, 2 / 3))
    Output
      <quantiles[9]>
      [1] [5.5]  [6.5]  [7.5]  [8.5]  [9.5]  [10.5] [11.5] [12.5] [13.5]
      # Quantile levels:  0.333 0.667 

---

    Code
      quantile_pred(matrix(seq(0.01, 1 - 0.01, length.out = 6), 3), c(0.2, 0.8))
    Output
      <quantiles[3]>
      [1] [0.304] [0.5]   [0.696]
      # Quantile levels:  0.2 0.8 

---

    Code
      tibble(qntls = v)
    Output
      # A tibble: 5 x 1
            qntls
        <qtls(4)>
      1     [8.5]
      2     [9.5]
      3    [10.5]
      4    [11.5]
      5    [12.5]

---

    Code
      quantile_pred(m, 1:4 / 5)
    Output
      <quantiles[5]>
      [1] [8.5]  [9.5]  [10.5] [11.5] [12.5]
      # Quantile levels:  0.2 0.4 0.6 0.8 

---

    Code
      one_quantile
    Output
      <quantiles[5]>
      [1] 1 2 3 4 5
      # Quantile levels:  0.556 

---

    Code
      tibble(qntls = one_quantile)
    Output
      # A tibble: 5 x 1
           qntls
        <qtl(1)>
      1        1
      2        2
      3        3
      4        4
      5        5

---

    Code
      quantile_pred(m, 5 / 9)
    Output
      <quantiles[5]>
      [1]  1 NA  3  4  5
      # Quantile levels:  0.556 

