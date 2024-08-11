test_that("count_aggregate counts and sums correctly", {
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

test_that('convert_tabular converts a semi-long table correctly', {
  semilong <- data.frame(
    Row = gl(3, 1, 12, labels = c('A', 'B', 'C')),
    Column = gl(4, 3, 12, labels = c('X', 'Y', 'Z', 'W')),
    n = 1:12,
    percent = c(0, 1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    suppress = c(FALSE, TRUE, TRUE, rep(FALSE, 9L))
  )

  expected_mcol <- dplyr::tibble(
    Row = gl(3, 1, 3, labels = c('A', 'B', 'C')),
    'X_No.' = 1:3,
    'X_%' = c(0, 1, 10),
    'Y_No.' = 4:6,
    'Y_%' = c(20, 30, 40),
    'Z_No.' = 7:9,
    'Z_%' = c(50, 60, 70),
    'W_No.' = 10:12,
    'W_%' = c(80, 90, 100)
  )

  tabular_mcol <- convert_tabular(
    semilong,
    Row ~ Column + .Measure,
    measures = c('No.' = 'n', '%' = 'percent')
  )

  expect_equal(tabular_mcol, expected_mcol)

  expected_mrow <- dplyr::tibble(
    Row = gl(3, 2, 6, labels = c('A', 'B', 'C')),
    Measure = forcats::as_factor(rep(c('No.', '%'), times = 3)),
    'X' = c(1, 0, 2, 1, 3, 10),
    'Y' = c(4, 20, 5, 30, 6, 40),
    'Z' = c(7, 50, 8, 60, 9, 70),
    'W' = c(10, 80, 11, 90, 12, 100)
  )

  tabular_mrow <- convert_tabular(
    semilong,
    Row + Measure ~ Column,
    measures = c('No.' = 'n', '%' = 'percent'),
    measurename = 'Measure'
  )

  expect_equal(tabular_mrow, expected_mrow)

  expected_nom <- dplyr::tibble(
    Row = gl(3, 1, 3, labels = c('A', 'B', 'C')),
    'X' = 1:3,
    'Y' = 4:6,
    'Z' = 7:9,
    'W' = 10:12,
  )

  tabular_nom <- convert_tabular(
    semilong,
    Row ~ Column,
    measures = c('No.' = 'n')
  )

  expect_equal(tabular_nom, expected_nom)
})

test_that('convert_tabular only supports one measure if not in formula', {
  semilong <- data.frame(
    Row = gl(3, 1, 12, labels = c('A', 'B', 'C')),
    Column = gl(4, 3, 12, labels = c('X', 'Y', 'Z', 'W')),
    n = 1:12,
    percent = c(0, 1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  )

  expect_error(
    convert_tabular(
      semilong,
      Row ~ Column,
      measures = c('No.' = 'n', '%' = 'percent')
    ),
    class = 'tabular-modtab-error'
  )
})
