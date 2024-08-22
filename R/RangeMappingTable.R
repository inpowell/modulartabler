#' R6 class representing a mapping table for numeric ranges.
#'
#' This mapping table class defines the structure for a single dimension of a
#' count table, mapping a continuous variable to ranges.
#'
#' @export
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
        relationship = 'many-to-many'
      )
      map <- dplyr::select(map, tidyselect::all_of(c(table_name, postmap_data_col)))

      # Other includes all length(cuts) values that are not in the map already
      if (!is.null(.other)) {
        map <- dplyr::bind_rows(map, rlang::list2(
          !!table_name := factor(.other),
          !!postmap_data_col := setdiff(seq_along(cuts), map[[postmap_data_col]])
        ))
      }

      # Total includes all values that are in the map
      if (!is.null(.total)) {
        map <- dplyr::bind_rows(map, rlang::list2(
          !!table_name := factor(.total),
          !!postmap_data_col := unique(map[[postmap_data_col]])
        ))
      }

      private$bounds_ <- bounds
      private$cuts_ <- cuts
      private$premap_data_col_ <- data_col
      # Call to BaseMappingTable
      super$initialize(map, raw_cols = postmap_data_col, table_cols = table_name)
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
