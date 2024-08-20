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


test_that("BaseMappingTable with empty nullspace evaluates cleanly", {
  map <- tibble(
    Map = gl(3, 1),
    raw = gl(3, 1)
  )
  MT <- BaseMappingTable$new(map, 'raw', 'Map')
  ns <- matrix(NA_real_, nrow = 0L, ncol = 3L)

  expect_identical(MT$nullspace, ns)
})
