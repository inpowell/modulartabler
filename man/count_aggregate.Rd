% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables_long.R
\name{count_aggregate}
\alias{count_aggregate}
\title{Count and sum into a long table}
\usage{
count_aggregate(MT, data, wt = NULL, ..., name = "n")
}
\arguments{
\item{MT}{The \code{MappingTable} object defining the structure of the table.}

\item{data}{The raw dataset to count and aggregate.}

\item{wt}{An optional column in \code{data} to sum records by. Passed to
\code{\link[dplyr:count]{dplyr::tally()}}. When \code{NULL} (the default), counts records. Otherwise,
uses \code{sum(wt)}.}

\item{...}{Passed to \link[dplyr:mutate-joins]{dplyr::right_join}}

\item{name}{The name of the count column to create, if it does not
already exist. Passed to \code{\link[dplyr:count]{dplyr::tally()}}.}
}
\value{
\code{count_aggregate} returns a table of aggregated counts in long
format. The structure of the output table will be \code{MT$mtab} with an extra
column for the counts (\code{n} by default if \code{name} is not specified).
}
\description{
\code{count_aggregate} counts records or sum weights from raw data with a mapping
table. It gives the count of records in each of the
output table groups. It wraps around \code{\link[dplyr:count]{dplyr::tally()}}, which allows
weighted sums instead of counts using the \code{wt} argument.
}
