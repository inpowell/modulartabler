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
#' @param conversion (Optional.) A function to convert all measures to a common
#'   type, such as `as.numeric` or `as.character`.
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
#' @importFrom data.table melt dcast
convert_tabular <- function(
    table,
    formula,
    measures = c('No.' = 'n'),
    ...,
    conversion = NULL,
    value_override) {
  .Value <- NULL # Trick R CMD check https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153

  table <- data.table::as.data.table(table)

  if (missing(value_override)) {
    value_override = '.Value'
  } else if (!(value_override %in% names(table))) {
    stop("value_override must be present in table")
  }

  # Convert columns to common type, if requested.
  if (!is.null(conversion)) {
    conversion <- match.fun(conversion)
    table[, c(measures) := lapply(.SD, conversion), .SDcols = measures]
  }

  # Convert semi-long to long (all measures in rows)
  table_long <- melt(
    table, measure.vars = measures,
    variable.name = '.Measure', variable.factor = TRUE,
    value.name = '.Value'
  )

  # Use nice provided names for measures
  mnn <- names(measures)
  if (is.null(mnn)) mnn <- measures # Handle unnamed measures vector
  mnn[!nzchar(mnn)] <- measures[!nzchar(mnn)] # Handle specific unnamed measures

  levels(table_long$.Measure) <- mnn[match(levels(table_long$.Measure), measures)]

  # Replace NaNs, etc. with NA
  table_long[is.na(.Value), .Value := NA]

  # Write to wide format
  dcast(table_long, formula, value.var = value_override, ...)
}
