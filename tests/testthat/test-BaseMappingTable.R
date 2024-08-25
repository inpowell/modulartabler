test_that("count_aggregate counts correctly for BaseMappingTables", {
  map <- data.frame(
    Map = factor(c(1L, 2L, 3L, 4L, rep(5L, 2), rep(6L, 4)),
                 labels = c(LETTERS[1:4], 'C+D', 'Total')),
    raw = factor(LETTERS[c(1L:4L, 3L:4L, 1L:4L)])
  )
  MT <- BaseMappingTable$new(map, 'raw', 'Map')

  rawdata <- dplyr::tibble(
    raw = factor(LETTERS[1L:4L])[rep(1:4, times = c(23, 29, 31, 37))]
  )

  expected <- dplyr::tibble(
    Map = factor(c('A', 'B', 'C', 'D', 'C+D', 'Total'), levels = levels(map$Map)),
    n = c(23L, 29L, 31L, 37L, 68L, 120L)
  )

  expect_equal(count_aggregate(MT, rawdata), expected)
})

test_that("count_aggregate works when table_cols = data_cols", {
  map <- data.frame(
    Map = factor(c(1L, 2L, 3L, 4L, rep(5L, 2), rep(6L, 4)),
                 labels = c(LETTERS[1:4], 'C+D', 'Total')),
    raw = factor(LETTERS[c(1L:4L, 3L:4L, 1L:4L)])
  )
  MT <- BaseMappingTable$new(map, raw_cols = 'raw', table_cols = 'Map', data_cols = 'Map')

  rawdata <- dplyr::tibble(
    Map = factor(LETTERS[1L:4L])[rep(1:4, times = c(23, 29, 31, 37))]
  )

  expected <- dplyr::tibble(
    Map = factor(c('A', 'B', 'C', 'D', 'C+D', 'Total'), levels = levels(map$Map)),
    n = c(23L, 29L, 31L, 37L, 68L, 120L)
  )

  expect_equal(MT$count_aggregate(rawdata), expected)
})


test_that("nullspace for BaseMappingTable is correct", {
  map <- tibble(
    Map = factor(c(1L, 2L, 3L, 4L, rep(5L, 2), rep(6L, 4)),
                 labels = c(LETTERS[1:4], 'C+D', 'Total')),
    raw = factor(LETTERS[c(1L:4L, 3L:4L, 1L:4L)])
  )
  MT <- BaseMappingTable$new(map, 'raw', 'Map')

  nullspace <- rbind(
    c(0, 0, 1, 1, -1, 0),
    c(1, 1, 1, 1, 0, -1)
  )

  expect_equal_rowspace(MT$nullspace, nullspace)
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
