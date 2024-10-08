#' @title R6 Class representing a mapping table.
#'
#' @description A mapping table defines the structure of a presentation table as a
#' many-to-many map from columns in a raw dataset, to presentation-ready
#' categories in an output counts table.
#'
#' This class is virtual -- it should only be instantiated with a
#' `BaseMappingTable` or `MultiMappingTable`.
#'
#' @export
#' @examples
#' try(MappingTable$new()) # Fails -- use a subclass instead
#'
MappingTable <- R6::R6Class(
  'MappingTable',
  public = list(
    #' @description `MappingTable$new()` will fail with an error.
    #' @param ... Ignored.
    initialize = function(...) {
      stop("MappingTable is virtual and must not be instantiated.")
    },

    #' @description Print a mapping table and return it invisibly
    #' @param ... Passed to the `print` method for `tibble`
    print = function(...) {
      cat('Mapping table:\n')
      if (isTRUE(all.equal(self$data_cols, self$raw_cols))) {
        cat(sprintf(
          "(%s) \u2192 (%s)\n", # Right arrow
          paste0(self$data_cols, collapse = ', '),
          paste0(self$table_cols, collapse = ', ')
        ))
      } else {
        cat(sprintf(
          "(%s) \u2192 (%s) \u2192 (%s)\n", # Right arrow
          paste0(self$data_cols, collapse = ', '),
          paste0(self$raw_cols, collapse = ', '),
          paste0(self$table_cols, collapse = ', ')
        ))
      }

      cat('\nMap:\n')
      print(self$map, ...)
      invisible(self)
    },

    #' @description Count records or sum weights from raw data with a mapping
    #'   table. `count_aggregate()` gives the count of records in each of the
    #'   output table groups. It wraps around [dplyr::tally()], which allows
    #'   weighted sums instead of counts using the `wt` argument.
    #'
    #' @param data The raw dataset to aggregate.
    #' @param wt An optional column in `data` to sum records by. Passed to
    #'   [dplyr::tally()]. When `NULL` (the default), counts records. Otherwise,
    #'   uses `sum(wt)`.
    #' @param ... Passed to [dplyr::right_join()].
    #' @param name The name of the count column to create, if it does not
    #'   already exist. Passed to [dplyr::tally()].
    #' @importFrom dplyr group_by across tally ungroup rename right_join arrange
    #' @importFrom tidyselect all_of
    count_aggregate  = function(data, wt = NULL, ..., name = 'n') {
      data <- self$preprocess(data)
      grpdata <- group_by(data, across(all_of(self$data_cols)))
      tallydata <- ungroup(tally(grpdata, wt = {{wt}}, name = {{name}}))

      renamevec <- setNames(nm = self$raw_cols, object = self$data_cols)
      renamed <- rename(tallydata, !!!renamevec)

      joined <- right_join(
        x = renamed,
        y = self$map,
        by = self$raw_cols,
        suffix = c('.raw', ''), # Ensure mapped columns retain name
        ...
      )

      n <- rlang::sym(name)
      grpjoin <- group_by(joined, across(all_of(self$table_cols)))
      tallyjoin <- ungroup(tally(grpjoin, wt = !!n, name = {{name}}))
      arrange(tallyjoin, across(all_of(self$table_cols)))
    },

    #' @description Pre-process data for counting and aggregating. The default
    #'   behaviour is to return the dataset unchanged, and this may be modified
    #'   by creating a subclass.
    #'
    #' @param data The dataset to prepare for counting and aggregating.
    preprocess = function(data) {
      data # Default: no modification
    }
  ),

  active = list(
    #' @field map The data frame used for mapping.
    map = function() {stop('map has not been implemented for MappingTable')},

    #' @field mtab The table-side data that corresponds to columns of the matrix
    #'   representation.
    mtab = function() {stop('mtab has not been implemented for MappingTable')},

    #' @field matrix A matrix representation of the mapping table that indicates
    #'   which raw values (in rows) are mapped to table cells (in columns).
    matrix = function() {stop('matrix has not been implemented for MappingTable')},

    #' @field raw_cols The names of columns in `map` that are joined with
    #'   preprocessed data.
    raw_cols = function() {stop('raw_cols has not been implemented for MappingTable')},

    #' @field data_cols The names of columns preprocessed data that are joined
    #'   with `map`.
    data_cols = function() {stop('data_cols has not been implemented for MappingTable')},

    #' @field table_cols The names of columns in the output dataset.
    table_cols = function() {stop('table_cols has not been implemented for MappingTable')},

    #' @field nullspace A matrix with rowspace equal to the kernel of the matrix
    #'   representation.
    nullspace = function() {
      M <- self$matrix

      # To find the nullspace of M, we find the SVD of M. The right singular
      # vectors beyond the rank of M form an orthonormal basis for ker(M).
      svdM <- svd(M, 0, ncol(M))
      dM <- zapsmall(svdM$d, digits = -log10(.Machine$double.eps)/2)
      rank <- sum(dM > 0)
      inds <- setdiff(seq_len(ncol(M)), seq_len(rank))
      nsM <- svdM$v[, inds, drop = FALSE]

      # Return early for full-rank matrix
      if (rank == ncol(M))
        return(t(nsM))

      # Aims to set rows in nsM corresponding to totals to have only -1 or 0
      qrM <- qr(M)
      pvt <- qrM$pivot
      zapsmall(
        t(nsM %*% -solve(nsM[pvt[inds], ])),
        digits = -log10(.Machine$double.eps)/2
      )
    }
  )
)

# BaseMappingTable --------------------------------------------------------

#' @title R6 class representing a mapping table for a single dimension.
#'
#' @description This mapping table class defines the structure for a single dimension of a
#' count table. It is defined by a potentially many-to-many map from raw data to
#' table categories.
#'
#' @export
#' @examples
#' # Create a mapping table that maps iris species to their abbreviated binomial
#' # nomenclature, and includes a total.
#' species_table <- data.frame(
#'   Species = factor(
#'     c(1L, 2L, 3L, rep(4L, times = 3L)),
#'     labels = c("I. setosa", "I. versicolor", "I. virginica", "Total")
#'   ),
#'   .rawspecies = factor(rep(c("setosa", "versicolor", "virginica"), times = 2L))
#' )
#' species_table
#'
#' SpeciesMap <- BaseMappingTable$new(
#'   species_table,
#'   raw_cols = ".rawspecies",
#'   table_cols = "Species",
#'   data_cols = "Species"
#' )
#' SpeciesMap
#'
#' # Get the counts in each category in long format
#' SpeciesMap$count_aggregate(iris)
#'
#' # Show the relationships in the output table. This matrix tells us that
#' # I. setosa + I. versicolor + I. virginica - Total = 0. This is
#' # necessary for preventing back-calculation in cell suppression.
#' SpeciesMap$nullspace
#' # Sometimes we need a more complicated table, which often requires some fiddly
#' # data manipulation.
#' library(dplyr)
#'
#' # In AER::Fertility, these are recorded as factors - we will recode later
#' ethtab <- expand.grid(
#'   afam = c(TRUE, FALSE),
#'   hispanic = c(TRUE, FALSE),
#'   other = c(TRUE, FALSE)
#' )
#'
#' # Build the output categories. We code as factor so it sorts properly in the
#' # output
#' ethtab <- ethtab |>
#'   mutate(Ethnicity = as.factor(case_when(
#'     afam & !hispanic & !other ~ 'African-American only',
#'     !afam & hispanic & !other ~ 'Hispanic only',
#'     !afam & !hispanic & !other ~ 'Caucasian only',
#'     TRUE ~ 'Other or multiple ethnicities'
#'   )))
#'
#' # Using factor() and bind_rows() means new levels get appended at the end
#' ethtab2 <- bind_rows(
#'   ethtab,
#'   # Subtotals for individual races
#'   filter(ethtab, afam) |> mutate(Ethnicity = factor('Any African-American')),
#'   filter(ethtab, hispanic) |> mutate(Ethnicity = factor('Any Hispanic')),
#'   filter(ethtab, other) |> mutate(Ethnicity = factor('Any other ethnicity')),
#'   # Grand total
#'   mutate(ethtab, Ethnicity = factor('Total'))
#' ) |>
#'   # Recode inputs to match original data
#'   mutate(across(
#'     where(is.logical),
#'     \(x) factor(x, levels = c(FALSE, TRUE), labels = c('no', 'yes'))
#'   ))
#'
#' # Now we can create the mapping table!
#' EthnicityMap <- BaseMappingTable$new(
#'   ethtab2,
#'   raw_cols = c('afam', 'hispanic', 'other'),
#'   table_cols = 'Ethnicity'
#' )
#'
#' # Use this mapping table to conduct some counts on ethnicity
#' data('Fertility', package = 'AER')
#' EthnicityMap$count_aggregate(Fertility)
BaseMappingTable <- R6::R6Class(
  'BaseMappingTable', inherit = MappingTable,
  public = list(
    #' @description Create a new mapping table object. The order of column names
    #'   in `raw_cols`, `table_cols`, and `data_cols` should match.
    #'
    #' @param map A data frame defining the structure of the presentation
    #'   table in terms of raw columns. Note that the raw columns must have the
    #'   same type and values in the raw dataset to count, so factors should
    #'   match to factors, character to character. This is particularly
    #'   important for ordered factors.
    #' @param raw_cols The names of raw columns in `map`.
    #' @param table_cols The names of presentation columns in `map`.
    #' @param data_cols The names of raw columns in the raw data. This is only
    #'   necessary if `raw_cols` does not match the names in the raw data.
    #'
    #' @return A new `MappingTable` object.
    initialize = function(map, raw_cols, table_cols, data_cols = raw_cols) {
      # if (!identical(length(raw_cols), length(table_cols)))
      #   stop('raw_cols and table_cols must have the same length')

      if (!identical(length(raw_cols), length(data_cols)))
        stop('raw_cols and data_cols must have the same length')

      if (!all(raw_cols %in% names(map)))
        stop('raw_cols must be names of columns in map')

      if (!all(table_cols %in% names(map)))
        stop('table_cols must be names of columns in map')

      if (length(intersect(raw_cols, table_cols)))
        stop('raw_cols and table_cols must not have any elements in common')

      private$.map <- tibble::as_tibble(map)
      private$.data_cols <- data_cols
      private$.rawside_cols <- raw_cols
      private$.tabside_cols <- table_cols
    }
  ),

  active = list(
    #' @field map The data frame used for mapping.
    map = function() {private$.map},

    #' @field mraw The raw-side data that correspond to rows of the matrix
    #'   representation.
    mraw = function() {
      private$.map |>
        dplyr::select(tidyselect::all_of(private$.rawside_cols)) |>
        dplyr::distinct() |>
        dplyr::arrange(dplyr::across(tidyselect::all_of(private$.rawside_cols)))
    },

    #' @field mtab The table-side data that corresponds to columns of the matrix
    #'   representation.
    mtab = function(){
      private$.map |>
        dplyr::select(tidyselect::all_of(private$.tabside_cols)) |>
        dplyr::distinct() |>
        dplyr::arrange(dplyr::across(tidyselect::all_of(private$.tabside_cols)))
    },

    #' @field matrix A matrix representation of the mapping table that indicates
    #'   which raw values (in rows) are mapped to table cells (in columns).
    #' @importFrom dplyr mutate cur_group_id
    #' @importFrom tidyselect all_of
    matrix = function() {
      idx <- private$.map |>
        mutate(.I = cur_group_id(), .by = all_of(private$.rawside_cols)) |>
        mutate(.J = cur_group_id(), .by = all_of(private$.tabside_cols))

      mat <- matrix(0L, nrow = max(idx$.I), ncol= max(idx$.J))
      mat[cbind(idx$.I, idx$.J)] <- 1L
      mat
    },

    #' @field raw_cols The names of columns in `map` that are joined with
    #'   preprocessed data.
    raw_cols = function() {private$.rawside_cols},

    #' @field data_cols The names of columns in the preprocessed data that are joined
    #'   with `map`.
    data_cols = function() {private$.data_cols},

    #' @field table_cols The names of columns in the output dataset.
    table_cols = function() {private$.tabside_cols}
  ),

  private = list(
    .map = tibble(),
    .data_cols = character(),
    .rawside_cols = character(),
    .tabside_cols = character()
  )
)
