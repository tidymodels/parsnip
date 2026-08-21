new_ordinal_translation <- function(
  fit_args,
  penalty = 0.1,
  eng_args = list()
) {
  list(
    args = list(
      penalty = rlang::new_quosure(penalty, rlang::empty_env())
    ),
    eng_args = eng_args,
    method = list(
      fit = list(args = fit_args)
    )
  )
}
