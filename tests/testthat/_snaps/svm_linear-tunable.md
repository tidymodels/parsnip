# tunable parameters for svm_linear + LiblineaR

    Code
      display_tunable_call_info(set_engine(svm_linear(), "LiblineaR"))
    Output
      cost                      | pkg: dials, fun: cost, range: c(-10.00,   5.00) | main
      margin                    | pkg: dials, fun: svm_margin | main

# tunable parameters for svm_linear + kernlab

    Code
      display_tunable_call_info(set_engine(svm_linear(), "kernlab"))
    Output
      cost                      | pkg: dials, fun: cost, range: c(-10.00,   5.00) | main
      margin                    | pkg: dials, fun: svm_margin | main

