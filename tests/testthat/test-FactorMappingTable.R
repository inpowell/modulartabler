test_that("nullspace with overlapping categories works", {
  MT <- FactorMappingTable$new(
    'Group', 'groupid',
    'A' = 'a',
    'B' = 'b',
    'C' = 'c',
    'D' = 'd',
    'C+D' = c('c', 'd'),
    'D+E' = c('d', 'e'), # NB 'e' does not appear on own
    .other = NULL,
    .total = 'Total'
  )

  expected_nullspace <- rbind(
    c(1, 1, 1, 0, 0, 1, -1), # A + B + C + (D+E) = Total
    c(0, 0, 1, 1, -1, 0, 0)  # C + D = (C+D)
  )
  expect_equal_rowspace(MT$nullspace, expected_nullspace)
})

test_that("counting with other/total arguments works", {
  exampledata <- data.frame(groupid = rep(
    c('a', 'b', 'c', 'd', 'e', NA),
    times = c(24, 8, 12, 36, 16, 6)
  ))

  # Neither other nor total
  MT_neither <- FactorMappingTable$new(
    'Group', 'groupid',
    'A' = 'a', 'B' = 'b', 'C' = 'c', 'D' = 'd',
    'C+D' = c('c', 'd'),
    .other = NULL, .total = NULL
  )
  exp_neither <- tibble(
    Group = forcats::as_factor(c('A', 'B', 'C', 'D', 'C+D')),
    n = c(24L, 8L, 12L, 36L, 48L)
  )
  expect_equal(MT_neither$count_aggregate(exampledata), exp_neither)

  # Total, but no other
  MT_total <- FactorMappingTable$new(
    'Group', 'groupid',
    'A' = 'a', 'B' = 'b', 'C' = 'c', 'D' = 'd',
    'C+D' = c('c', 'd'),
    .other = NULL, .total = 'Total'
  )
  exp_total <- tibble(
    Group = forcats::as_factor(c('A', 'B', 'C', 'D', 'C+D', 'Total')),
    n = c(24L, 8L, 12L, 36L, 48L, 80L)
  )
  expect_equal(MT_total$count_aggregate(exampledata), exp_total)

  # Other, but no total
  MT_other <- FactorMappingTable$new(
    'Group', 'groupid',
    'A' = 'a', 'B' = 'b', 'C' = 'c', 'D' = 'd',
    'C+D' = c('c', 'd'),
    .other = 'Other/Unknown', .total = NULL
  )
  exp_other <- tibble(
    Group = forcats::as_factor(c('A', 'B', 'C', 'D', 'C+D', 'Other/Unknown')),
    n = c(24L, 8L, 12L, 36L, 48L, 22L)
  )
  expect_equal(MT_other$count_aggregate(exampledata), exp_other)

  # Both other and total
  MT_both <- FactorMappingTable$new(
    'Group', 'groupid',
    'A' = 'a', 'B' = 'b', 'C' = 'c', 'D' = 'd',
    'C+D' = c('c', 'd'),
    .other = 'Other/Unknown', .total = 'Total'
  )
  exp_both <- tibble(
    Group = forcats::as_factor(c('A', 'B', 'C', 'D', 'C+D', 'Other/Unknown', 'Total')),
    n = c(24L, 8L, 12L, 36L, 48L, 22L, 102L)
  )
  expect_equal(MT_both$count_aggregate(exampledata), exp_both)
})
