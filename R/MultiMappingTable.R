#' R6 class representing a multidimensional mapping table.
#'
#' A multidimensional mapping table is the simplest way to build a table in
#' multiple dimensions. It contains a cell for each level of each category in
#' the input tables.
#'
#' @export
#' @md
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
    preprocess = function(data) { # Override
      fns <- purrr::map(private$.tables, function(x) x$preprocess)
      purrr::compose(!!!fns)(data)
    }
  ),

  active = list(
    #' @importFrom dplyr cross_join
    map = function() {
      private$.tables |>
        purrr::map(function(x) x$map) |>
        purrr::reduce(cross_join) |> # Cartesian join
        dplyr::relocate(tidyselect::all_of(self$table_cols))
    },
    mtab = function() {
      private$.tables |>
        purrr::map(function(x) x$mtab) |>
        purrr::reduce(cross_join) |> # Cartesian join
        dplyr::relocate(tidyselect::all_of(self$table_cols))
    },
    matrix = function() {
      private$.tables |>
        purrr::map(function(x) x$matrix) |>
        purrr::reduce(kronecker)
    },
    raw_cols = function() do.call(c, purrr::map(private$.tables, function(x) x$raw_cols)),
    data_cols = function() do.call(c, purrr::map(private$.tables, function(x) x$data_cols)),
    table_cols = function() do.call(c, purrr::map(private$.tables, function(x) x$table_cols)),
    join_clause = function() {
      exprs <- purrr::map(private$.tables, function(x) x$join_clause$exprs)
      dplyr::join_by(!!!unlist(exprs))
    },
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
