#' R6 class representing a multidimensional mapping table.
#'
#' A multidimensional mapping table is the simplest way to build a table in
#' multiple dimensions. It contains a cell for each level of each category in
#' the input tables.
#'
#' @export
#' @importFrom purrr map reduce
MultiMappingTable <- R6::R6Class(
  'MultiMappingTable', inherit = MappingTable,

  public = list(
    #' @description Create a new MultiMappingTable object
    #'
    #' @param ... One or more MappingTables, each representing a different
    #'   dimension of the desired table.
    initialize = function(...) {
      tabs <- list(...)
      if (!all(sapply(tabs, is, class2 = 'MappingTable')))
        stop('MultiMappingTables must be made from MappingTables')

      private$.tables <- tabs
    },

    #' @description Pre-process a dataset for counting and aggregating. The
    #'   method for MultiMappingTable passes the data through the preprocessing
    #'   functions for each of its subtables.
    #'
    #' @param data The dataset to prepare for counting and aggregation.
    preprocess = function(data) {
      fns <- purrr::map(private$.tables, function(x) x$preprocess)
      purrr::compose(!!!fns)(data)
    }
  ),

  active = list(
    #' @field map The data frame used for mapping.
    #' @importFrom dplyr cross_join relocate
    #' @importFrom purrr map reduce
    #' @importFrom tidyselect all_of
    map = function() {
      private$.tables |>
        map(function(x) x$map) |>
        reduce(cross_join) |> # Cartesian join
        relocate(all_of(self$table_cols))
    },
    #' @field mtab The table-side data that corresponds to columns of the matrix
    #'   representation.
    #' @importFrom dplyr cross_join relocate
    #' @importFrom purrr map reduce
    #' @importFrom tidyselect all_of
    mtab = function() {
      private$.tables |>
        map(function(x) x$mtab) |>
        reduce(cross_join) |> # Cartesian join
        relocate(all_of(self$table_cols))
    },
    #' @field matrix A matrix representation of the mapping table that indicates
    #'   which raw values (in rows) are mapped to table cells (in columns).
    #' @importFrom purrr map reduce
    matrix = function() {
      private$.tables |>
        map(function(x) x$matrix) |>
        reduce(kronecker)
    },
    #' @field raw_cols The names of columns in `map` that are joined with
    #'   preprocessed data.
    raw_cols = function() {
      do.call(c, purrr::map(private$.tables, function(x) x$raw_cols))
    },
    #' @field data_cols The names of columns preprocessed data that are joined
    #'   with `map`.
    data_cols = function() {
      do.call(c, purrr::map(private$.tables, function(x) x$data_cols))
    },
    #' @field table_cols The names of columns in the output dataset.
    table_cols = function() {do.call(c, purrr::map(private$.tables, function(x) x$table_cols))},

    #' @field join_clause A [dplyr::join_by()] object that describes how to join
    #'   the data (in `x`) to the mapping table (in `y`).
    join_clause = function() {
      exprs <- purrr::map(private$.tables, function(x) x$join_clause$exprs)
      dplyr::join_by(!!!unlist(exprs))
    },

    #' @field nullspace A matrix with rowspace equal to the kernel of the matrix
    #'   representation.
    nullspace = function() { # override
      ns <- matrix(nrow = 0L, ncol = 1L)
      nr <- 1L

      for (tab in rev(private$.tables)) {
        ns <- rbind(
          tab$matrix %x% ns,
          tab$nullspace %x% diag(nrow = nr)
        )
        nr <- nr * nrow(tab$mtab)
      }

      ns
    }
  ),

  private = list(
    .tables = list()
  )
)
