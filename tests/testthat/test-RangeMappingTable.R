test_that("nullspace with overlapping ranges works", {
  MTtotal <- RangeMappingTable$new(
    table_name = 'Age',
    data_col = 'age',
    "16-24" = c(16, 25),
    "16-34" = c(16, 35),
    "25-34" = c(25, 35),
    "35+" = c(35, Inf),
    .other = "Unknown",
    .total = "Total"
  )

  nullspace_total <- rbind(
    c(1, 0, 1, 1, 1, -1),
    c(1, -1, 1, 0, 0, 0)
  )

  expect_equal_rowspace(MTtotal$nullspace, nullspace_total)

  MTnototal <- RangeMappingTable$new(
    table_name = 'Age',
    data_col = 'age',
    "16-24" = c(16, 25),
    "16-34" = c(16, 35),
    "25-34" = c(25, 35),
    "35+" = c(35, Inf),
    .other = "Unknown",
    .total = NULL
  )

  nullspace_nototal <- rbind(c(1, -1, 1, 0, 0))

  expect_equal_rowspace(MTnototal$nullspace, nullspace_nototal)
})

test_that("count_aggregate in RangeMappingTable counts missings correctly", {
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

test_that("RangeMappingTable other/total args work modularly", {
  MT_total_null <- RangeMappingTable$new(
    table_name = 'Age',
    data_col = 'matage',
    "<20" = c(-Inf, 20),
    "20-34" = c(20, 35),
    "35+" = c(35, Inf),
    .other = "Unknown",
    .total = NULL
  )
  expect_equal_rowspace(MT_total_null$nullspace, matrix(nrow = 0, ncol = 4L))

  MT_other_null <- RangeMappingTable$new(
    table_name = 'Age',
    data_col = 'matage',
    "<20" = c(-Inf, 20),
    "20-34" = c(20, 35),
    "35+" = c(35, Inf),
    .other = NULL,
    .total = "Total"
  )
  expect_equal_rowspace(MT_other_null$nullspace, rbind(c(1, 1, 1, -1)))

  MT_both_null <- RangeMappingTable$new(
    table_name = 'Age',
    data_col = 'matage',
    "<20" = c(-Inf, 20),
    "20-34" = c(20, 35),
    "35+" = c(35, Inf),
    .other = NULL,
    .total = NULL
  )
  expect_equal_rowspace(MT_both_null$nullspace, matrix(nrow = 0, ncol = 3L))

  test_with_na <- data.frame(matage = c(0:120, rep(NA_real_, 5)))

  exp_both_null <- tibble(
    Age = forcats::as_factor(c('<20', '20-34', '35+')),
    n = c(20L, 15L, 86L)
  )

  exp_other_null <- tibble(
    Age = forcats::as_factor(c('<20', '20-34', '35+', 'Total')),
    n = c(20L, 15L, 86L, 121L)
  )

  exp_total_null <- tibble(
    Age = forcats::as_factor(c('<20', '20-34', '35+', 'Unknown')),
    n = c(20L, 15L, 86L, 5L)
  )

  expect_equal(MT_both_null$count_aggregate(test_with_na), exp_both_null)
  expect_equal(MT_other_null$count_aggregate(test_with_na), exp_other_null)
  expect_equal(MT_total_null$count_aggregate(test_with_na), exp_total_null)
})

test_that("non-contiguous ranges are supported in RangeMappingTable", {
  RMT <- RangeMappingTable$new(
    'Age', 'age',
    '<20' = c(0, 20),
    '35+' = c(35, Inf),
    .other = 'Other/Unknown',
    .total = "Total"
  )

  test <- data.frame(age = c(0:120, NA, NA))

  exp_counts <- dplyr::tibble(
    Age = forcats::as_factor(c('<20', '35+', 'Other/Unknown', 'Total')),
    n = c(20L, 86L, 17L, 123L)
  )

  expect_equal(RMT$count_aggregate(test), exp_counts)

  # These two matrices have the same rowspace if there exists an _invertible_
  # matrix X such that obs_ns = X %*% exp_ns
  obs_ns <- RMT$nullspace
  exp_ns <- rbind(c(1, 1, 1, -1))
  candidate <- t(qr.solve(t(exp_ns), t(obs_ns)))

  # This matrix is invertible?
  expect_no_error(solve(candidate))
  # This matrix satisfies the requirements
  expect_equal(obs_ns, candidate %*% exp_ns)
})


