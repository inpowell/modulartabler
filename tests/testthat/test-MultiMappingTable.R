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
