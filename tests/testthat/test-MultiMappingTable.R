test_that('nullspace for MultiMappingTable is correct', {
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

  # Commented rows are linearly dependent on other rows
  nullspace <- rbind(
    c(1, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), # X + Y = Total within A
    c(0, 0, 0, 1, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), # X + Y = Total within B
    c(0, 0, 0, 0, 0, 0, 1, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0), # X + Y = Total within C
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, -1, 0, 0, 0, 0, 0, 0), # X + Y = Total within D
  # c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, -1, 0, 0, 0), # X + Y = Total within C+D
  # c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, -1), # X + Y = Total within Total
    c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, -1, 0, 0), # A + B + C + D = Total within X
    c(0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, -1, 0), # A + B + C + D = Total within Y
    c(0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, -1), # A + B + C + D = Total within Total
    c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 0), # C + D = C+D within X
    c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, -1, 0, 0, 0, 0), # C + D = C+D within Y
    c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, -1, 0, 0, 0)  # C + D = C+D within Total
  )

  expect_equal_rowspace(MTM$nullspace, nullspace)
})

test_that('RangeMappingTables are compatible with MultiMappingTables', {
  map <- tibble(
    Map = factor(c(1L, 2L, rep(3L, 2)),
                 labels = c('X', 'Y', 'Total')),
    raw = factor(c('X', 'Y', 'X', 'Y'))
  )

  MT <- BaseMappingTable$new(map, 'raw', 'Map')

  RMT <- RangeMappingTable$new(
    "Continuous", "cont",
    "Low" = c(-Inf, 5),
    "Mid" = c(5, 10),
    "High" = c(10, 15),
    .other = NULL,
    .total = "Total"
  )

  MTM <- MultiMappingTable$new(MT, RMT)

  test <- tibble(raw = rep(c("X", "Y"), 5),
                 cont = rep(1:5, each = 2))

  expect_MTM <- tibble(
    Map = factor(c(rep("X", 4), rep("Y", 4), rep("Total", 4)),
                 levels = c("X", "Y", 'Total')),
    Continuous = factor(rep(c("Low", "Mid", "High", "Total"), 3),
                        levels = c("Low", "Mid", "High", "Total")),
    n = as.integer(rep(c(4, 1, 0, 5), 3) * rep(c(1, 1, 2), each = 4))
  )

  # Commented row is linearly dependent on other rows
  expect_ns <- rbind(
    c(1, 1, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0), # L + M + H = Total within X
    c(0, 0, 0, 0, 1, 1, 1, -1, 0, 0, 0, 0), # L + M + H = Total within Y
  # c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, -1), # L + M + H = Total within Total
    c(1, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0), # X + Y = Total within Low
    c(0, 1, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0), # X + Y = Total within Medium
    c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, -1, 0), # X + Y = Total within High
    c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, -1)  # X + Y = Total within Total
  )

  expect_equal(MTM$count_aggregate(test), expect_MTM)
  expect_equal_rowspace(MTM$nullspace, expect_ns)
})
