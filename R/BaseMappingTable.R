#' R6 Class representing a mapping table.
#'
#' A mapping table defines the structure of a presentation table. It defines
#' mappings from raw columns to columns in a presentation-ready table.
#'
#' This class is virtual -- it should only be instantiated with a
#' `BaseMappingTable` or `MultiMappingTable`.
#'
#' @export
MappingTable <- R6::R6Class(
  'MappingTable',
  public = list(
    #' @description `MappingTable$new()` will fail with an error.
    #' @param ... Ignored.
    initialize = function(...)
      stop("MappingTable is virtual and must not be instantiated."),

    #' @description Print a mapping table and return it invisibly
    #' @param nlines (Ignored.) Maximum number of mappings to print.
    #' @param nobjs (Ignored.) Maximum number of objects to print per line.
    #' @param ... Passed to the `print` method for `tibble`
    #' @md
    print = function(nlines = 10L, nobjs = 2L, ...) {
      cat('Mapping table:\n')
      cat('  Map:\n')
      print(self$map, ...)
      print(self$join_clause)
      invisible(self)
    },

    #' @description Count records in a data set, grouping by mapping table
    #'   categories.
    #' @param data The raw dataset to count and aggregate.
    #' @param ... Passed to [dplyr::inner_join]
    #' @md
    count_aggregate = function(data, ...) {
      data <- self$preprocess(data)
      grpraw <- dplyr::group_by(
        data,
        dplyr::across(tidyselect::all_of(self$data_cols))
      )
      summraw <- dplyr::summarise(grpraw, n = dplyr::n(), .groups = 'drop')

      joined <- dplyr::right_join(
        x = summraw,
        y = self$map,
        by = self$join_clause,
        ...
      )

      grouped <- dplyr::group_by(
        joined,
        dplyr::across(tidyselect::all_of(self$table_cols))
      )
      dplyr::summarise(grouped, n = sum(n, na.rm = TRUE), .groups = 'drop')
    },

    #' @description Pre-process data for counting and aggregating
    #'
    #' @param data The dataset to prepare for counting and aggregating.
    #' @md
    preprocess = function(data) {
      data # Default: no modification
    }
  ),

  active = list(
    #' @field map The data frame used for mapping
    map = function() stop('map has not been implemented for MappingTable'),

    #' @field mtab The table-side data that corresponds to columns of the matrix
    #'   representation.
    mtab = function() stop('mtab has not been implemented for MappingTable'),

    #' @field matrix A matrix representation of the mapping table that indicates
    #'   which raw values (in rows) are mapped to table cells (in columns).
    matrix = function() stop('matrix has not been implemented for MappingTable'),

    #' @field raw_cols The names of columns corresponding to raw data in `map`
    raw_cols = function() stop('raw_cols has not been implemented for MappingTable'),

    #' @field data_cols The names of columns in the raw data that correspond to
    #'   `raw_cols`
    data_cols = function() stop('data_cols has not been implemented for MappingTable'),

    #' @field table_cols The names of columns corresponding to display table in
    #'   `map`
    table_cols = function() stop('table_cols has not been implemented for MappingTable'),

    #' @field join_clause A [dplyr::join_by()] object that describes how to join
    #'   the data (in `x`) to the mapping table (in `y`).
    join_clause = function() stop('join_clause has not been implemented for MappingTable'),

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

#' R6 class representing a mapping table for a single dimension.
#'
#' This mapping table class defines the structure for a single dimension of a
#' count table.
#'
#' @export
#' @md
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
    #' @md
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
    map = function() private$.map,

    #' @field mraw The raw-side data that correspond to rows of the matrix
    #'   representation.
    mraw = function() {
      private$.map |>
        dplyr::select(tidyselect::all_of(private$.rawside_cols)) |>
        dplyr::distinct() |>
        dplyr::arrange(dplyr::across(tidyselect::all_of(private$.rawside_cols)))
    },
    mtab = function(){
      private$.map |>
        dplyr::select(tidyselect::all_of(private$.tabside_cols)) |>
        dplyr::distinct() |>
        dplyr::arrange(dplyr::across(tidyselect::all_of(private$.tabside_cols)))
    },
    matrix = function() private$to_matrix(),
    raw_cols = function() private$.rawside_cols,
    data_cols = function() private$.data_cols,
    table_cols = function() private$.tabside_cols,
    join_clause = function() {
      exprs <- purrr::map2(private$.data_cols, private$.rawside_cols,
                           \(x, y) call('==', x, y))
      dplyr::join_by(!!!exprs)
    }
  ),

  private = list(
    .map = tibble(),
    .data_cols = character(),
    .rawside_cols = character(),
    .tabside_cols = character(),
    #' @importFrom dplyr inner_join mutate row_number
    to_matrix = function() {
      idx <- self$map |>
        inner_join(mutate(self$mraw, I = row_number()), by = self$raw_cols) |>
        inner_join(mutate(self$mtab, J = row_number()), by = self$table_cols)

      mat <- matrix(0L, nrow = nrow(self$mraw), ncol = nrow(self$mtab))
      mat[cbind(idx$I, idx$J)] <- 1L
      mat
    }
  )
)
