test_that('correct results for glm_grouped()', {
  ucb_weighted <- as.data.frame(UCBAdmissions)
  ucb_weighted$Freq <- as.integer(ucb_weighted$Freq)

  ucb_long <- tidyr::uncount(ucb_weighted, Freq)

  ungrouped <- glm(Admit ~ Gender + Dept, data = ucb_long, family = binomial)

  expect_no_condition(
    grouped <- glm_grouped(Admit ~ Gender + Dept, data = ucb_weighted, weights = ucb_weighted$Freq)
  )

  expect_equal(grouped$df.null, 11)
})
