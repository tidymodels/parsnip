# one-hot encoding contrasts

    Code
      contr_one_hot(character(0))
    Condition
      Error in `contr_one_hot()`:
      ! A character vector for `n` cannot be empty.

---

    Code
      contr_one_hot(-1)
    Condition
      Error in `contr_one_hot()`:
      ! `n` must be a whole number larger than or equal to 1, not the number -1.

---

    Code
      contr_one_hot(list())
    Condition
      Error in `contr_one_hot()`:
      ! `n` must be a whole number, not an empty list.

---

    Code
      contr_one_hot(2, contrast = FALSE)
    Condition
      Warning:
      `contrasts = FALSE` not implemented for `contr_one_hot()`.
    Output
        1 2
      1 1 0
      2 0 1

---

    Code
      contr_one_hot(2, sparse = TRUE)
    Condition
      Warning:
      `sparse = TRUE` not implemented for `contr_one_hot()`.
    Output
        1 2
      1 1 0
      2 0 1

