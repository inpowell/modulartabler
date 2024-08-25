# Two matrices A and E have the same rowspace if there exists an invertible
# matrix X such that A = X %*% E. We will also assume that rows are linearly
# independent.
expect_equal_rowspace <- function(object, expected, ...) {
  act <- quasi_label(rlang::enquo(object), arg = 'object')
  exp <- quasi_label(rlang::enquo(expected), arg = 'expected')

  if ((nact <- nrow(act$val)) != (nexp <- nrow(exp$val))) {
    message <- glue::glue("{act$lab} has {nact} rows, while {exp$lab} has {nexp}.")
    fail(message)
  }

  if ((nact <- ncol(act$val)) != (nexp <- ncol(exp$val))) {
    message <- glue::glue("{act$lab} has {nact} columns, while {exp$lab} has {nexp}.")
    fail(message)
  }

  if (nrow(act$val) == 0L) {
    succeed("Nullspaces are both trivial.")
    return(invisible(act$val))
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
