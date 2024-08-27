#' @title Basic Mapping Table for discrete categorisations
#'
#' @description This mapping table describes the structure of a single dimension
#'   of a counts table, where categories are described by one or more discrete
#'   levels.
#'
#' @export
#' @examples
#' exampledata <- data.frame(groupid = rep(
#'   c('a', 'b', 'c', 'd', 'e', NA),
#'   times = c(24, 8, 12, 36, 16, 6)
#' ))
#'
#' # If we set .other = NULL, then 'e' and NA are not counted, even in Total
#' MT_no_other <- FactorMappingTable$new(
#'   'Group', 'groupid',
#'   'A' = 'a',
#'   'B' = 'b',
#'   'C' = 'c',
#'   'D' = 'd',
#'   'C+D' = c('c', 'd'),
#'   .other = NULL,
#'   .total = "Total"
#' )
#'
#' MT_no_other
#' MT_no_other$count_aggregate(exampledata)
#'
#' # Otherwise, 'e' and NA are included in both Other/Unknown and Total
#' MT_other <- FactorMappingTable$new(
#'   'Group', 'groupid',
#'   'A' = 'a',
#'   'B' = 'b',
#'   'C' = 'c',
#'   'D' = 'd',
#'   'C+D' = c('c', 'd'),
#'   .other = "Other/Unknown",
#'   .total = "Total"
#' )
#'
#' MT_other
#' MT_other$count_aggregate(exampledata)
FactorMappingTable <- R6::R6Class(
  'FactorMappingTable', inherit = BaseMappingTable,
  public = list(
    #' @description Create a new mapping table object for discrete groupings.
    #'
    #' @param table_name The name of the column to map to. This should generally
    #'   be human-readable.
    #' @param data_col The name of the column in the dataset to map from.
    #' @param ... Category definitions, written as named arguments. The names
    #'   become the human-readable output categories, and the values of each
    #'   argument are the specific values in the data that all get mapped to
    #'   that category.
    #' @param .other The name of the category to use for values not included in
    #'   `...`, including missing values. Set to `NULL` to exclude missing
    #'   values from the mapping and totals.
    #' @param .total The name of the category to use for the total mapping,
    #'   which includes all records included in the rest of the map. Set to
    #'   `NULL` to omit the total category.
    #'
    #' @return A new [MappingTable] object.
    #'
    #' @importFrom cli cli_abort
    #' @importFrom dplyr bind_rows
    #' @importFrom rlang dots_list list2
    #' @importFrom tibble enframe
    #' @importFrom tidyr unnest
    #' @importFrom tidyselect all_of
    #' @importFrom vctrs vec_cast_common
    initialize = function(
        table_name,
        data_col,
        ...,
        .other = "Other/Unknown",
        .total = "Total") {
      dl <- dots_list(..., .named = TRUE, .homonyms = 'keep')
      categories <- vec_cast_common(!!!dl, .arg = '')

      miss <- purrr::map_lgl(categories, function(x) any(is.na(x)))
      if (any(miss)) {
        missnames <- names(categories)[miss]
        info <- c(
          'x' = "Raw values in {.var ...} in {.cls FactorMappingTable} must not be {.val {NA}}",
          '!' = "Argument{?s} {.arg {missnames}} {?has/have} missing values."
        )
        cli_abort(info, class = c('missing-FMT-creation-error', 'FMT-creation-error', 'modulartabler-error'))
      }

      postpcol <- paste0('.', data_col)

      nestmap <- enframe(categories, name = table_name, value = postpcol)
      nestmap[[table_name]] <- factor(nestmap[[table_name]], levels = names(categories))
      map <- unnest(nestmap, cols = all_of(postpcol))

      if (!is.null(.other)) {
        map <- bind_rows(map, list2(!!table_name := factor(.other), !!postpcol := NA))
      }

      if (!is.null(.total)) {
        map <- bind_rows(map, list2(
          !!table_name := factor(.total),
          !!postpcol := unique(map[[postpcol]])
        ))
      }

      private$unique_categories <- unique(unlist(categories))
      private$preprocess_data_col <- data_col

      # Call BaseMappingTable$new
      super$initialize(
        map,
        table_cols = table_name,
        raw_cols = postpcol,
        data_cols = postpcol
      )
    },
    #' @description Pre-process data for discrete mappings. For
    #'   FactorMappingTable, `preprocess(data)` creates a new column that maps
    #'   all variables that are not given in `initialize()` to a missing value.
    #'
    #' @param data The dataset to prepare for counting and aggregating.
    #' @importFrom dplyr mutate case_when
    #' @importFrom rlang sym .data .env
    preprocess = function(data) {
      post_col <- sym(self$data_cols)
      pre_col <- sym(private$preprocess_data_col)
      ucat <- private$unique_categories

      mutate(data, !!post_col := case_when(
        !!pre_col %in% .env$ucat ~ !!pre_col,
        TRUE ~ NA
      ))
    }
  ),
  private = list(
    unique_categories = character(), # All raw data values used in table
    preprocess_data_col = character() # Name of column in raw data before preprocessing
  )
)
