test_that("BaseMappingTable bindings work as expected", {
  map <- tibble(
    Map = factor(c(1L, 2L, 3L, 4L, rep(5L, 2), rep(6L, 4)),
                 labels = c(LETTERS[1:4], 'C+D', 'Total')),
    raw = factor(LETTERS[c(1L:4L, 3L:4L, 1L:4L)])
  )

  rawside <- tibble(raw = factor(LETTERS[1L:4L]))
  tabside <- tibble(Map = factor(1L:6L, labels = c(LETTERS[1:4], 'C+D', 'Total')))

  matrix <- rbind(
    c(1L, 0L, 0L, 0L, 0L, 1L),
    c(0L, 1L, 0L, 0L, 0L, 1L),
    c(0L, 0L, 1L, 0L, 1L, 1L),
    c(0L, 0L, 0L, 1L, 1L, 1L)
  )

  nullspace <- rbind(
    c(0, 0, 1, 1, -1, 0),
    c(1, 1, 1, 1, 0, -1)
  )

  MT <- BaseMappingTable$new(map, 'raw', 'Map')

  expect_equal(MT$map, map)
  expect_equal(MT$matrix, matrix)
  expect_equal(MT$mraw, rawside)
  expect_equal(MT$mtab, tabside)
  expect_equal(MT$data_cols, 'raw')
  expect_equal(MT$raw_cols, 'raw')
  expect_equal(MT$table_cols, 'Map')
  expect_equal(MT$join_clause, dplyr::join_by('raw' == 'raw')) # Character is okay
  expect_equal(MT$nullspace, nullspace)
})

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

test_that('MultiMappingTable bindings work correctly', {
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

  MTM <- MultiMappingTable$new(MT1, MT2)

  rawside <- tibble(
    raw1 = gl(4, 1, 8, labels = LETTERS[1:4]),
    raw2 = gl(2, 4, 8, labels = c("X", "Y"))
  )

  tabside <- tibble(
    Map1 = gl(6, 3, 18, labels = c(LETTERS[1:4], 'C+D', 'Total')),
    Map2 = gl(3, 1, 18, labels = c('X', 'Y', 'Total'))
  )

  matrix <- rbind(
    c(1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L), # A, X
    c(0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L), # A, Y
    c(0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L), # B, X
    c(0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L), # B, Y
    c(0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 1L), # C, X
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, 1L), # C, Y
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 1L), # D, X
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 1L)  # D, Y
  )

  raw_cols <- c('raw1', 'raw2')
  data_cols <- c('raw1', 'raw2')
  tab_cols <- c('Map1', 'Map2')

  expect_equivalent(MTM$map[seq_len(nrow(map2)), c('Map2', 'raw2')], map2) # map2 varies fastest
  expect_equal(MTM$matrix, matrix)
  expect_equal(MTM$mtab, tabside)
  expect_equal(MTM$data_cols, data_cols)
  expect_equal(MTM$raw_cols, raw_cols)
  expect_equal(MTM$table_cols, tab_cols)
  expect_equal(MTM$join_clause, dplyr::join_by(
    'raw1' == 'raw1',
    'raw2' == 'raw2'
  ))
  expect_true(all(matrix %*% t(MTM$nullspace) == 0))

  RMT1 <- RangeMappingTable$new("Continuous", "cont",
                                "Low" = c(-Inf, 5),
                                "Mid" = c(5, 10),
                                "High" = c(10, 15),
                                .other = NULL,
                                .total = "Total")

  MTM2 <- MultiMappingTable$new(MT2, RMT1)

  map3 <- tibble(raw2 = rep(c("X", "Y"), 5),
                 cont = rep(1:5, each = 2))

  expect_MTM2 <- tibble(Map2 = factor(c(rep("X", 4), rep("Y", 4), rep("Total", 4)),
                                      levels = c("X", "Y", 'Total')),
                        Continuous = factor(rep(c("Low", "Mid", "High", "Total"), 3),
                                            levels = c("Low", "Mid", "High", "Total")),
                        n = as.integer(rep(c(4, 1, 0, 5), 3) * rep(c(1, 1, 2), each = 4)))

  expect_MTM2_ns <- matrix(c(1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, -1, 0,
                             0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1,
                             0, 0, -1, 0, 0, 0, 1, 1, 1, -1, 0, 0, 0, 1, 1, 0, -1, 0, 0, 1,
                             1, 0, 0, -1, 0, -1, -1, 0, 0, 0, -1), ncol = 12)

  expect_equal(MTM2$count_aggregate(map3), expect_MTM2)
  expect_equal(MTM2$nullspace, expect_MTM2_ns)

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
