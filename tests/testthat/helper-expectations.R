# Two matrices A and E have the same rowspace iff there exists an invertible
# matrix X such that A = X %*% E. We will also assume that rows are linearly
# independent.
expect_equal_rowspace <- function(object, expected, ...) {
  act <- quasi_label(rlang::enquo(object), arg = 'object')
  exp <- quasi_label(rlang::enquo(expected), arg = 'expected')

  if ((nact <- nrow(act$val)) != (nexp <- nrow(exp$val))) {
    message <- glue::glue("{act$lab} has {nact} rows, while {exp$lab} has {nexp}.")
    fail(message)
  }

  # A = X %*% E iff t(A) = t(E) %*% t(X) -- this gets the least-squares solution
  candidate <- t(qr.solve(t(exp$val), t(act$val)))

  # Is this matrix invertible?
  invres <- try(solve(candidate), silent = TRUE)
  if (inherits(invres, 'try-error')) {
    message <- glue::glue("Matrix between {act$lab} and {exp$lab} is not invertible.")
    fail(message)
  }

  # Is A = X %*% E, to some sensible tolerance?
  expect_equal(act$val, candidate %*% exp$val, ...)
}

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
})
