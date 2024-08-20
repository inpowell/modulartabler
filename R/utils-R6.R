# Needed to solve R CMD check problem
#' @import R6
NULL

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
