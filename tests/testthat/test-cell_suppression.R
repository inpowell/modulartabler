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

test_that("Solves highly dimensional tables that may return non-integer LP solutions", {
  # as.logical(solution) turns any non-zero x*, such as floating points, into TRUE
  # See Issue #17 for further details
  create_maps <- function(colname, max = 5) {
    BaseMappingTable$new(tibble::tibble(!!colname := c(letters[1:max],
                                                       letters[1:max]),
                                        !!paste0(colname, "2") := c(LETTERS[1:max],
                                                                    rep("Total", max))),
                         colname, paste0(colname, "2"))
  }

  map <- do.call(MultiMappingTable$new, purrr::map(LETTERS[1:3], ~ create_maps(.x, max = 4)))

  n <- c(1L, 35L, 37L, 31L, 104L, 2L, 36L, 27L, 9L, 74L, 38L, 37L, 24L,  29L, 128L,
         22L, 25L, 35L, 2L, 84L, 63L, 133L, 123L, 71L, 390L, 40L, 30L, 31L, 6L,
         107L, 3L, 39L, 28L, 24L, 94L, 32L, 15L, 18L, 2L, 67L, 35L, 10L, 10L,
         32L, 87L, 110L, 94L, 87L, 64L, 355L, 21L, 6L, 9L, 24L, 60L, 16L, 36L,
         30L, 23L, 105L, 39L, 22L, 19L, 35L, 115L, 9L, 16L, 17L, 37L, 79L, 85L,
         80L, 75L, 119L, 359L, 33L, 19L, 34L, 33L, 119L, 10L, 24L, 29L, 25L, 88L,
         36L, 6L, 26L, 17L, 85L, 22L, 29L, 26L, 16L, 93L, 101L, 78L, 115L, 91L,
         385L, 95L, 90L, 111L, 94L, 390L, 31L, 135L, 114L, 81L, 361L, 145L, 80L,
         87L, 83L, 395L, 88L, 80L, 88L, 87L, 343L, 359L, 385L, 400L, 345L, 1489L)

  soln <- c(T, T, F, F, F, T, F, F, T, F, F, F, F, F, F, F, T, F, T, F, F, F, F,
            F, F, T, T, F, F, F, T, F, F, T, F, F, T, F, T, F, F, F, F, F, F, F,
            F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,
            F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, T, F, T, F, F, T,
            F, T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,
            F, F, F, F, F, F, F, F, F, F)

  expect_identical(determine_cell_suppression(n, map$nullspace), soln)
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
