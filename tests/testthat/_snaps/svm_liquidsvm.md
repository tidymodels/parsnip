# primary arguments

    Code
      basic_liquidSVM$method$fit$args
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $folds
      [1] 1
      
      $threads
      [1] 0
      

---

    Code
      rbf_sigma_liquidSVM$method$fit$args
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $gammas
      <quosure>
      expr: ^0.2
      env:  empty
      
      $folds
      [1] 1
      
      $threads
      [1] 0
      

# engine arguments

    Code
      translate(liquidSVM_scale, "liquidSVM")$method$fit$args
    Output
      $x
      missing_arg()
      
      $y
      missing_arg()
      
      $scale
      <quosure>
      expr: ^FALSE
      env:  empty
      
      $predict.prob
      <quosure>
      expr: ^TRUE
      env:  empty
      
      $threads
      <quosure>
      expr: ^2
      env:  empty
      
      $gpus
      <quosure>
      expr: ^1
      env:  empty
      
      $folds
      [1] 1
      

