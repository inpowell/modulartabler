test_that("RangeMappingTable bindings work as expected", {
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

  # Expected mapping table
  map <- tibble(
    Age = forcats::as_factor(c('<20', '20-34', '35+', 'Unknown', rep('Total', 4L))),
    .matage = c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L)
  )
  expect_equal(MT$map, map)

  # Expected join clause
  join_clause <- dplyr::join_by('.matage' == '.matage')
  expect_equal(MT$join_clause, join_clause)

  # Expected matrix
  mat <- rbind(
    c(1, 0, 0, 0, 1), # <20
    c(0, 1, 0, 0, 1), # 20-34
    c(0, 0, 1, 0, 1), # 35+
    c(0, 0, 0, 1, 1)  # Unknown
  )
  expect_equal(MT$matrix, mat)

  # Expected nullspace
  ns <- rbind(c(1, 1, 1, 1, -1))
  expect_equal(MT$nullspace, ns)

  # Expected column names
  expect_equal(MT$data_cols,  '.matage')
  expect_equal(MT$raw_cols,   '.matage')
  expect_equal(MT$table_cols, 'Age')

  ## Expected table skeleton
  tabside <- tibble(
    Age = forcats::as_factor(c('<20', '20-34', '35+', 'Unknown', 'Total'))
  )
  expect_equal(MT$mtab, tabside)

  ## Expected raw stand-in
  rawside <- tibble(
    .matage = c(1L, 2L, 3L, 4L)
  )
  expect_equal(MT$mraw, rawside)
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

  MT_other_null <- RangeMappingTable$new(
    table_name = 'Age',
    data_col = 'matage',
    "<20" = c(-Inf, 20),
    "20-34" = c(20, 35),
    "35+" = c(35, Inf),
    .other = NULL,
    .total = "Total"
  )

  MT_both_null <- RangeMappingTable$new(
    table_name = 'Age',
    data_col = 'matage',
    "<20" = c(-Inf, 20),
    "20-34" = c(20, 35),
    "35+" = c(35, Inf),
    .other = NULL,
    .total = NULL
  )

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


