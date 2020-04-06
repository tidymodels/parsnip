# discrim

<details>

* Version: 0.0.1
* Source code: https://github.com/cran/discrim
* Date/Publication: 2019-10-11 12:10:02 UTC
* Number of recursive dependencies: 108

Run `revdep_details(,"discrim")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [90m  1. [39mtestthat::expect_error(...)
      [90m  2. [39mdiscrim::discrim_regularized()
      [90m 10. [39mparsnip::set_engine(., "monday")
      [90m 15. [39mparsnip:::check_engine(object)
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════
      [ OK: 411 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 5 ]
      1. Failure: api errors (@test-flexible.R#106) 
      2. Failure: api errors (@test-linear-fda.R#107) 
      3. Failure: api errors (@test-linear-lda.R#132) 
      4. Failure: api errors (@test-naive-Bayes.R#195) 
      5. Failure: api errors (@test-rda.R#108) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

