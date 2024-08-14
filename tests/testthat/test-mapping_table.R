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

test_that('MultiMappingTable nullspace is correct', {
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

  expect_true(all(matrix %*% t(MTM$nullspace) == 0))
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
