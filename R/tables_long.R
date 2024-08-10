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
#'   The `.Measure` variable can be used where there are multiple measures.
#'   Passed to `data.table::melt`.
#' @param measures Columns that correspond to measures wanted in the output
#'   table. The names of the vector will be used as "pretty" names in the output
#'   table.
#' @param ... Passed to `data.table::melt`.
#' @param value_override (Optional.) If a supplementary table is needed that
#'   does not come from the measure columns (e.g. whether a cell needs to be
#'   suppressed), this argument overrides the value used when converting to
#'   tabular format.
#'
#' @return `convert_tabular` returns a `data.table` in visual tabular form.
#'   Column names are determined by `data.table::melt`, which defaults to the
#'   various dimensions separated by an underscore.
#' @export
#' @md
#' @importFrom tidyr build_wider_spec pivot_wider_spec unite replace_na
#' @importFrom rlang .env
#' @importFrom dplyr mutate select arrange across
#' @importFrom tidyselect all_of
convert_tabular <- function(
    table,
    formula,
    measures = c('No.' = 'n'),
    ...,
    value_override) {
  rowterms <- all.vars(formula[[2]])
  colterms <- all.vars(formula[[3]])
  hasMeasure <- '.Measure' %in% colterms
  colterms <- setdiff(colterms, '.Measure')
  spec <- build_wider_spec(
    table,
    names_from = all_of(colterms),
    values_from = all_of(unname(measures))
  )

  specmod <- spec |>
    mutate(.valname = names(measures)[match(.value, measures)]) |>
    unite('.name', c(all_of(colterms), '.valname'), sep = '_', remove = FALSE) |>
    select(-.valname) |>
    arrange(across(all_of(colterms)), match(.value, .env$measures))

  visualtab <- pivot_wider_spec(table, specmod, id_cols = all_of(rowterms))

  # Replace NaNs, etc. with NA
  mutate(visualtab, across(all_of(.env$specmod$.name), \(x) replace_na(x, NA)))
}
