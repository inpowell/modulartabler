test_that('expect_equal_rowspace works', {
  test <- rbind(
    c(1, 1, 1, 1, 0, -1),
    c(0, 0, 1, 1, -1, 0)
  )
  equal <- t(qr.Q(qr(t(test)))) # test, but orthonormal
  nequal <- rbind(
    c(1, 1, 1, 1, 0, -1),
    c(0, 0, 1, 0, -1, 0)
  )

  expect_success(expect_equal_rowspace(test, equal))
  expect_failure(expect_equal_rowspace(test, nequal))

  expect_success(expect_equal_rowspace(matrix(nrow = 0, ncol = 10), matrix(nrow = 0, ncol = 10)))
  expect_failure(expect_equal_rowspace(matrix(nrow = 0, ncol = 10), matrix(nrow = 0, ncol = 5)))
})
