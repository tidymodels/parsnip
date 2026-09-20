# glm_grouped() errors informatively on non-two-level outcomes

    Code
      glm_grouped(Dept ~ Gender + Admit, data = ucb_weighted, weights = ucb_weighted$
        Freq)
    Condition
      Error in `glm_grouped()`:
      ! The response column Dept should be a two-level factor.
      i It has 6 levels: "A", "B", "C", "D", "E", and "F".

