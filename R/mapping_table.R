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


# RangeMappingTable -------------------------------------------------------

#' R6 class representing a mapping table for numeric ranges.
#'
#' This mapping table class defines the structure for a single dimension of a
#' count table, mapping a continuous variable to ranges.
#'
#' @export
#' @md
RangeMappingTable <- R6::R6Class(
  'RangeMappingTable', inherit = BaseMappingTable,

  public = list(
    #' @description Create a new mapping table object for numeric ranges.
    #'
    #' @param table_name The name of the column to map to. This should generally
    #'   be human-readable.
    #' @param data_col The name of the numeric column in the dataset to map
    #'   from.
    #' @param ... Range definitions for mapping categories, written as named
    #'   arguments. Each argument should be a length-2 numeric vector showing
    #'   the minimum and maximum range.
    #' @param .other The name of the category to use for missing values and
    #'   ranges not included in `...`. Set to NULL to exclude missing values
    #'   from the mapping and totals.
    #' @param .total The name of the category to use for the total mapping,
    #'   which includes all records. Set to NULL to omit the total category.
    #' @param bounds Define which sides of each range should be included. At
    #'   this stage, only semi-open ranges are allowed, with `[)` keeping the
    #'   lower limit but excluding the upper limit, and `(]` keeping the upper
    #'   limit but excluding the lower limit.
    #'
    #' @return A new `MappingTable` object.
    #' @md
    #'
    #' @examples
    #' # Maps the `age` column into `Age` categories of <20, 20-34, and 35+,
    #' # with separate Unknown and Total categories.
    #' AgeMap <- RangeMappingTable$new(
    #'   table_name = 'Age',
    #'   data_col = 'age',
    #'   "<20" = c(-Inf, 20),
    #'   "20-34" = c(20, 35),
    #'   "35+" = c(35, Inf),
    #'   .other = "Unknown",
    #'   .total = "Total",
    #'   bounds = '[)' # include lower bound, exclude upper bound
    #' )
    #' AgeMap
    #'
    #' refdata <- data.frame(age = c(0:120, NA))
    #' AgeMap$count_aggregate(refdata)
    initialize = function(
        table_name,
        data_col,
        ...,
        .other = "Other/Unknown",
        .total = "Total",
        bounds = c('[)', '(]')) {
      ranges <- rlang::dots_list(..., .named = TRUE, .homonyms = 'keep')
      cuts <- unique(sort(unlist(ranges)))

      if (any(wronglength <- lengths(ranges) != 2)) {
        wrongnames <- names(ranges)[wronglength]
        info <- c(
          'x' = "Ranges in {.var ...} in {.cls RangeMappingTable} must be length 2",
          '!' = "Argument{?s} {.arg {wrongnames}} {?is/are} the wrong length",
          'i' = "Use {.code c(NA, NA)} for missing values"
        )
        cli::cli_abort(info)
      }

      nonnum <- !purrr::map_lgl(ranges, is.numeric)
      if (any(nonnum)) {
        wrongnames <- names(ranges)[nonnum]
        info <- c(
          'x' = "Ranges in {.var ...} in {.cls RangeMappingTable} must be numeric",
          '!' = "Argument{?s} {.arg {wrongnames}} {?is/are} the wrong type"
        )
        cli::cli_abort(info)
      }

      miss <- purrr::map_lgl(ranges, function(x) any(is.na(x)))
      if (any(miss)) {
        missnames <- names(ranges)[miss]
        info <- c(
          'x' = "Ranges in {.var ...} in {.cls RangeMappingTable} must not be {.val {NA_integer_}}",
          '!' = "Argument{?s} {.arg {missnames}} {?has/have} missing extents"
        )
        cli::cli_abort(info)
      }

      bounds <- match.arg(bounds)

      if (length(table_name) != 1L) {
        cli::cli_abort(c(
          "{.arg table_name} must be length 1 in {.cls RangeMappingTable}"
        ))
      }

      if (length(data_col) != 1L) {
        cli::cli_abort(c(
          "{.arg data_col} must be length 1 in {.cls RangeMappingTable}"
        ))
      }

      postmap_data_col <- paste0('.', data_col)
      uniqueranges <- tibble(
        unique.left = cuts[-length(cuts)],
        unique.right = cuts[-1L]
      )
      uniqueranges[[postmap_data_col]] <- seq_len(nrow(uniqueranges))

      tableranges <- tibble::enframe(ranges, name = table_name, value = 'range') |>
        dplyr::mutate(dplyr::across(
          tidyselect::all_of(table_name),
          forcats::as_factor
        )) |>
        tidyr::unnest_wider(col = 'range', names_sep = '.')

      map <- dplyr::inner_join(
        uniqueranges,
        tableranges,
        by = dplyr::join_by(within(
          unique.left, unique.right,
          range.1, range.2
        )),
        relationship = 'many-to-many',
        unmatched = 'error'
      )
      map <- dplyr::select(map, tidyselect::all_of(c(table_name, postmap_data_col)))

      other_map <- if (!is.null(.other)) {
        rlang::list2(!!table_name := factor(.other),
                     !!postmap_data_col := length(cuts)) # Other/Unknown
      } else NULL

      total_map <- if(!is.null(.total)) {
        rlang::list2(!!table_name := factor(.total),
                     !!postmap_data_col := seq_len(nrow(uniqueranges) + !is.null(.other))) # Total
      } else NULL

      tmap <- dplyr::bind_rows(
        map,
        other_map,
        total_map
      )

      private$bounds_ <- bounds
      private$cuts_ <- cuts
      private$premap_data_col_ <- data_col
      # Call to BaseMappingTable
      super$initialize(tmap, raw_cols = postmap_data_col, table_cols = table_name)
    },

    preprocess = function(data) {
      rawsym <- rlang::sym(private$premap_data_col_)
      mapsym <- rlang::sym(super$data_cols)
      left.open <- identical(private$bounds_, '(]')

      ### We use case_when and misdirection to work with lazy tibbles, like from
      ### dtplyr or dbplyr
      dplyr::mutate(
        data,
        !!mapsym := !!findInterval_to_casewhen(
          !!rawsym,
          vec = private$cuts_,
          left.open = left.open
        )
      )
    }
  ),

  private = list(
    bounds_ = logical(),
    cuts_ = numeric(),
    premap_data_col_ = character()
  )
)

#' Call like findInterval to get an [rlang::expr] to an equivalent
#' [dplyr::case_when] statement.
#' @noRd
findInterval_to_casewhen <- function(x, vec, left.open) {
  x <- rlang::ensym(x)
  if (left.open) {
    ineq <- purrr::map2(
      vec[-length(vec)], vec[-1L],
      function(left, right) rlang::expr(
        !!left < !!x & !!x <= !!right
      )
    )
  } else {
    ineq <- purrr::map2(
      vec[-length(vec)], vec[-1L],
      function(left, right) rlang::expr(
        !!left <= !!x & !!x < !!right
      )
    )
  }

  ff <- purrr::map2(ineq, seq_along(ineq), function(x, y) call('~', x, y))

  rlang::call2(
    'case_when',
    !!!ff,
    call('~', TRUE, length(ineq) + 1L),
    .ns = 'dplyr'
  )
}

# MultiMappingTable -------------------------------------------------------

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

#' @noRd
.R6imports <- function() {
  # Functions that are used in our R6 classes, but would otherwise cause a
  # WARNING in R CMD check.
  #
  # See https://forum.posit.co/t/new-r-cmd-check-note-in-r-4-2-0-for-imports-field/143153/4
  forcats::as_factor
  tidyr::unnest_wider
  tidyselect::all_of
  invisible(NULL)
}
