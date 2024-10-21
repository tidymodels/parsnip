test_that('one-hot encoding contrasts', {

  contr_mat <- contr_one_hot(12)
  expect_equal(colnames(contr_mat), paste(1:12))
  expect_equal(rownames(contr_mat), paste(1:12))
  expect_true(all(apply(contr_mat, 1, sum) == 1))
  expect_true(all(apply(contr_mat, 2, sum) == 1))

  chr_contr_mat <- contr_one_hot(letters[1:12])
  expect_equal(colnames(chr_contr_mat), letters[1:12])
  expect_equal(rownames(chr_contr_mat), letters[1:12])
  expect_true(all(apply(chr_contr_mat, 1, sum) == 1))
  expect_true(all(apply(chr_contr_mat, 2, sum) == 1))

  expect_snapshot(contr_one_hot(character(0)), error = TRUE)
  expect_snapshot(contr_one_hot(-1), error = TRUE)
  expect_snapshot(contr_one_hot(list()), error = TRUE)
  expect_snapshot(contr_one_hot(2, contrast = FALSE))
  expect_snapshot(contr_one_hot(2, sparse = TRUE))

})
