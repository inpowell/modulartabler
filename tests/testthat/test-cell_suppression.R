test_that("determine_cell_suppression correctly suppresses cells", {
  withr::local_package('dplyr')
  skip_if_not_installed('ROI.plugin.highs')

  test_tab <- dplyr::tibble(
    Region = gl(3, 3, 9, labels = LETTERS[1:3]),
    Activity = gl(3, 1, 9, labels = c('I', 'II', 'III')),
    n = c(20, 50, 10,  8, 19, 22,  17, 32, 12),
    suppress = c(FALSE, FALSE, FALSE,  TRUE, FALSE, TRUE,  TRUE, FALSE, TRUE)
  )

  test_rtot <- summarise(
    test_tab,
    Region = 'Total',
    n = sum(n),
    suppress = FALSE,
    .by = Activity
  )
  test_atot <- summarise(
    test_tab,
    Activity = 'Total',
    n = sum(n),
    suppress = FALSE,
    .by = Region
  )
  test_tot <- summarise(
    test_tab,
    Activity = 'Total',
    Region = 'Total',
    n = sum(n),
    suppress = FALSE
  )

  test <- bind_rows(test_tab, test_rtot, test_atot, test_tot)

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

test_that("Correctly deals with constraints hidden by prior cells", {
  # A prior implementation did not conduct a final pass over cells,
  # resulting in one cell implementing a later constraint that interfered
  # with a prior set of constraints that passed initial solutions before
  # all cells had been assessed
  n <- c(262L, 78L, 340L, 110L, 59L, 169L, 52L, 16L, 68L, 54L, 16L, 70L, 97L, 17L,
         114L, 90L, 37L, 127L, 1L, 3L, 4L, 0L, 0L, 0L, 666L, 226L, 892L)
  ns <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                 1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
                 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, -1, 0, 0, 0,
                 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0,
                 0, 0, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0,
                 0, 0, -1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0,
                 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, -1, 0, 0, 0,
                 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                 0, 0, 1, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0,
                 0, 0, -1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, -1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
                 -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, -1), ncol = 27)

  soln <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)

  expect_identical(determine_cell_suppression(n, ns), soln)
})

test_that("suppress_secondary deals with an empty nullspace (#9)", {
  data <- seq(0, 10, by = 2)
  suppress <- c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)
  nullspace <- matrix(NA_real_, nrow = 0, ncol = 6)
  expect_equal(
    suppress_secondary(data, suppress, nullspace),
    suppress
  )
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
