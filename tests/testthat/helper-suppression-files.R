# Read and write tab-delimited files for saving large suppression tests
write_test_suppression_file <- function(suppress_pattern, counts, nullspace, filename) {

  ns <- t(nullspace)
  colnames(ns) <- paste0("NS", seq_len(ncol(ns)))

  tab <- cbind(suppress_pattern,
               counts,
               ns)

  write.table(tab,
              file = testthat::test_path(filename),
              row.names = FALSE, quote = FALSE, sep = "\t")

}

read_test_suppression_file <- function(filename) {

  tab <- read.table(testthat::test_path(filename),
                    row.names = NULL, quote = "",
                    header = TRUE, sep = "\t")

  ns <- t(as.matrix(tab[, grep("^NS", names(tab))]))
  rownames(ns) <- NULL

  list(suppress_pattern = as.logical(tab$suppress_pattern),
       counts = as.integer(tab$counts),
       nullspace = ns)

}
