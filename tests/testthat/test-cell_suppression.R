library(data.table)

test_that("determine_cell_suppression correctly suppresses cells", {
  skip_if_not_installed('ROI.plugin.highs')

  test_tab <- data.table(
    Region = gl(3, 3, 9, labels = LETTERS[1:3]),
    Activity = gl(3, 1, 9, labels = c('I', 'II', 'III')),
    n = c(20, 50, 10,  8, 19, 22,  17, 32, 12),
    suppress = c(FALSE, FALSE, FALSE,  TRUE, FALSE, TRUE,  TRUE, FALSE, TRUE)
  )
  test_rtot <- test_tab[, .(Region = 'Total', n = sum(n), suppress = FALSE),
                        by = .(Activity)]
  test_atot <- test_tab[, .(Activity = 'Total', n = sum(n), suppress = FALSE),
                        by = .(Region)]
  test_tot <- test_tab[, .(Region = 'Total', Activity = 'Total', n = sum(n), suppress = FALSE)]

  test <- rbind(test_tab, test_rtot, test_atot, test_tot)

  nullspace <- rbind(
    c(1, 0, 0,  1, 0, 0,  1, 0, 0,  -1,  0,  0,   0,  0,  0,   0),
    c(0, 1, 0,  0, 1, 0,  0, 1, 0,   0, -1,  0,   0,  0,  0,   0),
    c(0, 0, 1,  0, 0, 1,  0, 0, 1,   0,  0, -1,   0,  0,  0,   0),
    c(1, 1, 1,  0, 0, 0,  0, 0, 0,   0,  0,  0,  -1,  0,  0,   0),
    c(0, 0, 0,  1, 1, 1,  0, 0, 0,   0,  0,  0,   0, -1,  0,   0),
    c(0, 0, 0,  0, 0, 0,  1, 1, 1,   0,  0,  0,   0,  0, -1,   0),
    c(0, 0, 0,  0, 0, 0,  0, 0, 0,   1,  1,  1,   0,  0,  0,  -1),
    c(0, 0, 0,  0, 0, 0,  0, 0, 0,   0,  0,  0,   1,  1,  1,  -1)
  )

  # Confirm
  stopifnot(all(nullspace %*% test$n == 0L))

  soln <- with(test, determine_cell_suppression(N = n, nullspace, small_max = 9L))
  testthat::expect_equal(soln, test$suppress, check.attributes = FALSE)
})

test_that("suppress_secondary aborts if bounds are too tight", {
  skip_if_not_installed('ROI.plugin.highs')

  # If we have a table N = c(A = 2, B = 4, Total = 6) and set attacker bounds
  # LB = UB = N, then the attacker knows 0 <= A <= 4.
  N <- c(A = 2, B = 4, Total = 6)
  suppress <- c(TRUE, FALSE, FALSE)
  nullspace <- rbind(c(1, 1, -1))

  # A UPL of 3 means that minimum upper attacker guess must be A + 3 = 5 > 4
  expect_error(
    suppress_secondary(N, suppress, nullspace, LB = N, UB = N, UPL = 3L, SPL = 0L),
    class = c('UPL-error', 'bounds-error')
  )
  expect_equal(
    suppress_secondary(N, suppress, nullspace, LB = N, UB = N, UPL = 2L, SPL = 0L),
    c(TRUE, TRUE, FALSE)
  )

  # A LPL of 3 means that maximum lower attacker guess must be A - 3 = -1 < 0
  expect_error(
    suppress_secondary(N, suppress, nullspace, LB = N, UB = N, LPL = 3L, SPL = 0L),
    class = c('LPL-error', 'bounds-error')
  )
  expect_equal(
    suppress_secondary(N, suppress, nullspace, LB = N, UB = N, LPL = 2L, SPL = 0L),
    c(TRUE, TRUE, FALSE)
  )

  # A SPL of 5 means that the distance between upper and lower attacker guesses
  # must be at least 5, but 4 - 0 = 4 < 5
  expect_error(
    suppress_secondary(N, suppress, nullspace, LB = N, UB = N, SPL = 5L),
    class = c('SPL-error', 'bounds-error')
  )
  expect_equal(
    suppress_secondary(N, suppress, nullspace, LB = N, UB = N, SPL = 4L),
    c(TRUE, TRUE, FALSE)
  )
})
