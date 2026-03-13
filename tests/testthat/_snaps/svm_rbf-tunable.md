# tunable parameters for svm_rbf + kernlab

    Code
      display_tunable_call_info(set_engine(svm_rbf(), "kernlab"))
    Output
      cost                      | pkg: dials, fun: cost, range: c(-10.00,   5.00) | main
      rbf_sigma                 | pkg: dials, fun: rbf_sigma | main
      margin                    | pkg: dials, fun: svm_margin | main

# tunable parameters for svm_rbf + liquidSVM

    Code
      display_tunable_call_info(set_engine(svm_rbf(), "liquidSVM"))
    Condition
      Warning:
      The `engine` argument of `set_engine()` cannot be liquidSVM as of parsnip 0.1.6.
      i The liquidSVM package is no longer available on CRAN.
    Output
      cost                      | pkg: dials, fun: cost | main
      rbf_sigma                 | pkg: dials, fun: rbf_sigma | main

