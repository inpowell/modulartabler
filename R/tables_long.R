#' Count and sum into a long table
#'
#' `count_aggregate` counts and aggregates raw data into the structure
#' determined by the provided mapping table `MT`.
#'
#' @param MT The `MappingTable` object defining the structure of the table.
#' @param data The raw dataset to count and aggregate.
#' @param ... Passed to [dplyr::right_join]
#'
#' @return `count_aggregate` returns a table of aggregated counts in long
#'   format. The structure of the output table will be `MT$mtab` with an extra
#'   column `n` for the counts.
#'
#' @export
#' @md
count_aggregate <- function(MT, data, ...) {
  if (!inherits(MT, 'MappingTable')) {
    cli::cli_abort("{.var MT} must be a {.cls MappingTable} object in {.fun count_aggregate}.")
  }
  MT$count_aggregate(data, ...)
}

#' Convert count data to tabular form
#'
#' `convert_tabular` takes a table in semi-long format (where each row
#' corresponds to a cell, but each cell can have multiple measures), and returns
#' it in tabular format.
#'
#' @param table A data frame in semi-long format. Each dimension of the table
#'   should have a corresponding column, but there can be multiple columns for
#'   various measures.
#' @param formula A formula describing the visual structure of the table. The
#'   LHS of the formula corresponds to rows, and the RHS corresponds to columns.
#'   The `.Measure` variable (or alternative in `measurename`) can be used where
#'   there are multiple measures.
#' @param measures Columns that correspond to measures wanted in the output
#'   table. The names of the vector will be used as "pretty" names in the output
#'   table.
#' @param measurename The placeholder name in `formula` for distinguishing
#'   measures, where multiple measures are given. This name is also used as the
#'   descriptor of measures if used as a row group.
#'
#' @return `convert_tabular` returns a tibble in visual tabular form.
#' @export
#' @md
#' @importFrom tidyr pivot_longer build_wider_spec pivot_wider_spec unite
#'   replace_na
#' @importFrom rlang .data .env
#' @importFrom dplyr mutate select arrange across
#' @importFrom tidyselect all_of
#'
#' @examples
#' wbsumm <- dplyr::summarise(
#'   warpbreaks,
#'   mean = mean(breaks), sd = sd(breaks),
#'   .by = c(wool, tension)
#' )
#' wbsumm
#'
#' # Show wool type in rows, tension and mean/std dev in columns
#' convert_tabular(
#'   wbsumm,
#'   wool ~ tension + .Measure,
#'   measures = c(Mean = 'mean', 'Std. Dev.' = 'sd')
#' )
#'
#' # Show measure as largest row group with header Statistic
#' convert_tabular(
#'   wbsumm,
#'   Statistic + wool ~ tension,
#'   measures = c(Mean = 'mean', 'Std. Dev.' = 'sd'),
#'   measurename = 'Statistic'
#' )
#'
#' # Just show mean
#' convert_tabular(
#'   wbsumm,
#'   tension ~ wool,
#'   measures = 'mean'
#' )
convert_tabular <- function(
    table,
    formula,
    measures = c('No.' = 'n'),
    measurename = '.Measure') {
  rowterms <- all.vars(formula[[2]])
  colterms <- all.vars(formula[[3]])

  if (measurename %in% colterms) {
    hasMeasure <- TRUE
    colterms <- setdiff(colterms, measurename)
  } else if (measurename %in% rowterms) {
    # Need to pivot measures from columns to rows
    table <- pivot_longer(
      table,
      cols = all_of(unname(measures)),
      names_to = measurename,
      names_transform = \(x) factor(x, levels = measures, labels = names(measures)),
      values_to = '.value'
    )
    measures <- '.value'
    hasMeasure <- FALSE
  } else {
    if (length(measures) > 1L)
      cli::cli_abort(
        '{.var measures} must have length 1, or {.var {measurename}} must be specified in {.var formula}.',
        class = c('tabular-modtab-error', 'modtab-error')
      )
    hasMeasure <- FALSE
  }

  spec <- build_wider_spec(
    table,
    names_from = all_of(colterms),
    values_from = all_of(unname(measures))
  )

  if (hasMeasure) {
    spec <- spec |>
      mutate(.valname = names(.env$measures)[match(.data$.value, .env$measures)]) |>
      unite('.name', c(all_of(.env$colterms), '.valname'), sep = '_', remove = FALSE) |>
      select(-'.valname') |>
      arrange(across(all_of(colterms)), match(.data$.value, .env$measures))
  }

  visualtab <- pivot_wider_spec(table, spec, id_cols = all_of(rowterms))

  visualtab <- arrange(visualtab, across(all_of(rowterms)))

  # Replace NaNs, etc. with NA
  mutate(visualtab, across(all_of(.env$spec$.name), \(x) replace_na(x, NA)))
}
