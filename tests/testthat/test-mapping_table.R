test_that("BaseMappingTable counting and nullspace is correct", {
  withr::local_package('dplyr')
  map <- tibble(
    Map = factor(c(1L, 2L, 3L, 4L, rep(5L, 2), rep(6L, 4)),
                 labels = c(LETTERS[1:4], 'C+D', 'Total')),
    raw = factor(LETTERS[c(1L:4L, 3L:4L, 1L:4L)])
  )
  MT <- BaseMappingTable$new(map, 'raw', 'Map')

  test <- tibble(
    raw = forcats::as_factor(rep(c('A', 'B', 'C', 'D'), times = 1:4))
  )

  expected_count <- tibble(
    Map = forcats::as_factor(c('A', 'B', 'C', 'D', 'C+D', 'Total')),
    n = c(1L, 2L, 3L, 4L, 7L, 10L)
  )

  expect_equal(MT$count_aggregate(test), expected_count)

  # We don't need these matrices to be equal, we just need them to have the same
  # rowspace. Equivalently, an invertible matrix X exists s.t. ns_obs = X %*% ns_exp
  ns_obs <- MT$nullspace
  ns_exp <- rbind(
    c(1, 1, 1, 1, 0, -1),
    c(0, 0, 1, 1, -1, 0)
  )
  candidate <- t(qr.solve(t(ns_exp), t(ns_obs)))

  # This matrix is invertible?
  expect_no_error(solve(candidate))
  # This matrix satisfies the requirements
  expect_equal(ns_obs, candidate %*% ns_exp)
})

test_that("RangeMappingTable nullspace is correct", {
  # Observed mapping table
  MT <- RangeMappingTable$new(
    table_name = 'Age',
    data_col = 'matage',
    "<20" = c(-Inf, 20),
    "20-34" = c(20, 35),
    "35+" = c(35, Inf),
    .other = "Unknown",
    .total = "Total"
  )

  # Expected nullspace
  ns <- rbind(c(1, 1, 1, 1, -1))
  expect_equal(MT$nullspace / MT$nullspace[1], ns)
})

test_that("RangeMappingTable counts correctly with missings", {
  MT <- RangeMappingTable$new(
    table_name = 'Age',
    data_col = 'matage',
    "<20" = c(-Inf, 20),
    "20-34" = c(20, 35),
    "35+" = c(35, Inf),
    .other = 'Unknown',
    .total = 'Total'
  )

  test_0na <- data.frame(matage = 0:120)
  exp_0na <- tibble(
    Age = forcats::as_factor(c('<20', '20-34', '35+', 'Unknown', 'Total')),
    n = c(20L, 15L, 86L, 0L, 121L)
  )
  expect_equal(MT$count_aggregate(test_0na), exp_0na)

  test_1na <- data.frame(matage = c(0:120, NA))
  exp_1na <- tibble(
    Age = forcats::as_factor(c('<20', '20-34', '35+', 'Unknown', 'Total')),
    n = c(20L, 15L, 86L, 1L, 122L)
  )
  expect_equal(MT$count_aggregate(test_1na), exp_1na)

  test_2na <- data.frame(matage = c(0:120, NA, NA))
  exp_2na <- tibble(
    Age = forcats::as_factor(c('<20', '20-34', '35+', 'Unknown', 'Total')),
    n = c(20L, 15L, 86L, 2L, 123L)
  )
  expect_equal(MT$count_aggregate(test_2na), exp_2na)

})

test_that('MultiMappingTable nullspace is equivalent to BaseMappingTable', {
  withr::local_package('dplyr')
  map1 <- tibble(
    Map1 = factor(c(1L, 2L, 3L, 4L, rep(5L, 2), rep(6L, 4)),
                 labels = c(LETTERS[1:4], 'C+D', 'Total')),
    raw1 = factor(LETTERS[c(1L:4L, 3L:4L, 1L:4L)])
  )

  map2 <- tibble(
    Map2 = factor(c(1L, 2L, rep(3L, 2)),
                  labels = c('X', 'Y', 'Total')),
    raw2 = factor(c('X', 'Y', 'X', 'Y'))
  )

  MT1 <- BaseMappingTable$new(map1, 'raw1', 'Map1')
  MT2 <- BaseMappingTable$new(map2, 'raw2', 'Map2')

  MMT <- MultiMappingTable$new(MT1, MT2)
  BMT <- BaseMappingTable$new(
    cross_join(map1, map2),
    c('raw1', 'raw2'),
    c('Map1', 'Map2')
  )

  # We don't need these matrices to be equal, we just need them to have the same
  # rowspace. Equivalently, an invertible matrix X exists s.t. ns_obs = X %*%
  # ns_exp
  ns_obs <- MMT$nullspace
  ns_exp <- MMT$nullspace

  candidate <- t(qr.solve(t(ns_exp), t(ns_obs)))

  # This matrix is invertible?
  expect_no_error(solve(candidate))
  # This matrix satisfies the requirements?
  expect_equal(ns_obs, candidate %*% ns_exp)
})

test_that("BaseMappingTable with empty nullspace evaluates cleanly", {
  map <- tibble(
    Map = gl(3, 1),
    raw = gl(3, 1)
  )
  MT <- BaseMappingTable$new(map, 'raw', 'Map')
  ns <- matrix(NA_real_, nrow = 0L, ncol = 3L)

  expect_identical(MT$nullspace, ns)
})
