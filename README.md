
<!-- README.md is generated from README.Rmd. Please edit that file -->

# modulartabler

<!-- badges: start -->

[![R-CMD-check](https://github.com/inpowell/modulartabler/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/inpowell/modulartabler/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

modulartabler categorises records in a dataset and presents counts in
each category in tabular format. Each table of counts is described by a
mapping table, which can convert raw data to tabular format. Mapping
tables can correspond to a single dimension, or be combined to form a
multi-dimensional table.

When presenting tables, sometimes cells with small counts must be
suppressed, and other cells might need to be suppressed to prevent
back-calculation. Mapping tables in modulartabler record the
relationships within its table, and the package implements an algorithm
to solve the cell suppression problem.

## Installation

You can install the development version of modulartabler like so:

``` r
remotes::install_github('inpowell/modulartabler')
```

## Example

In this example, we will use modulartabler to examine efficiency of
vehicles in the `mtcars` dataset.

``` r
library(modulartabler)
data("mtcars")
```

First, we want to classify efficiency using the `mpg` column. To do
this, we construct a `RangeMappingTable` that lists the efficiency in
miles per galleon. We will categorise these as less than 20 mpg
(\<20.0), at least 20 but less than 25 mpg (20.0-24.9), and at least 25
mpg (25.0+). We will also add a subtotal for cars with at least 20 mpg
(20.0+).

``` r
MPG <- RangeMappingTable$new(
  'Miles per galleon', 'mpg',
  "<20.0" = c(0, 20),
  "20.0-24.9" = c(20, 25),
  "25.0+" = c(25, Inf),
  "20.0+" = c(20, Inf),
  .other = "Unknown",
  .total = 'Total'
)  
```

Now we can use this mapping table to count the number of cars by their
mileage.

``` r
MPG$count_aggregate(mtcars)
#> # A tibble: 6 × 2
#>   `Miles per galleon`     n
#>   <fct>               <int>
#> 1 <20.0                  18
#> 2 20.0-24.9               8
#> 3 25.0+                   6
#> 4 20.0+                  14
#> 5 Unknown                 0
#> 6 Total                  32
```

We may also be interested in whether cars in this dataset are more
efficient depending on whether the are automatic or manual. We can
construct a mapping table for transmission using `BaseMappingTable`.

``` r
am_map <- data.frame(
  Transmission = factor(c('Automatic', 'Manual', rep('Total', 2L))),
  am =                  c(          0,        1,            0, 1 )
)
Transmission <- BaseMappingTable$new(
  am_map, raw_cols = 'am', table_cols = 'Transmission'
)
```

Using this mapping table, we can see that a bit over half the cars in
our data are automatic.

``` r
Transmission$count_aggregate(mtcars)
#> # A tibble: 3 × 2
#>   Transmission     n
#>   <fct>        <int>
#> 1 Automatic       19
#> 2 Manual          13
#> 3 Total           32
```

However, to answer our question about whether efficiency is affected by
mileage, we need the cross-tabulation of these two dimensions. We can
create a mapping table to get this cross-tabulation using a
`MultiMappingTable`.

``` r
CrossMpgTransmission <- MultiMappingTable$new(Transmission, MPG)
```

Then, we are able to calculate counts in each category.

``` r
crosstab <- CrossMpgTransmission$count_aggregate(mtcars)
crosstab
#> # A tibble: 18 × 3
#>    Transmission `Miles per galleon`     n
#>    <fct>        <fct>               <int>
#>  1 Automatic    <20.0                  15
#>  2 Automatic    20.0-24.9               4
#>  3 Automatic    25.0+                   0
#>  4 Automatic    20.0+                   4
#>  5 Automatic    Unknown                 0
#>  6 Automatic    Total                  19
#>  7 Manual       <20.0                   3
#>  8 Manual       20.0-24.9               4
#>  9 Manual       25.0+                   6
#> 10 Manual       20.0+                  10
#> 11 Manual       Unknown                 0
#> 12 Manual       Total                  13
#> 13 Total        <20.0                  18
#> 14 Total        20.0-24.9               8
#> 15 Total        25.0+                   6
#> 16 Total        20.0+                  14
#> 17 Total        Unknown                 0
#> 18 Total        Total                  32
```

While this table has all the information we need, it would be more
appropriate to have it in wide format for readablity. We can do this
using `convert_tabular()`.

``` r
convert_tabular(crosstab, `Miles per galleon` ~ Transmission)
#> Key: <Miles per galleon>
#>    Miles per galleon Automatic Manual Total
#>               <fctr>     <int>  <int> <int>
#> 1:             <20.0        15      3    18
#> 2:         20.0-24.9         4      4     8
#> 3:             25.0+         0      6     6
#> 4:             20.0+         4     10    14
#> 5:           Unknown         0      0     0
#> 6:             Total        19     13    32
```

This shows all the information we need, including that manual cars tend
to have better mileage.

If this table referred to people, then it might be prudent to suppress
small cells. In particular, we might want to suppress the cell with
count 3.

``` r
crosstab$suppress <- 0L < crosstab$n & crosstab$n <= 3L

crosstab$n.primary <- crosstab$n
crosstab$n.primary[crosstab$suppress] <- NA
```

When we look at this table though, it is clear we can easily recover the
value of the suppressed cell. Indeed, the cell’s row total is 18, and
$18 - 15 = 3$. Also, its column total is 13, and the complementary cell
for 20+ mpg is 10, and $13 - 10 = 3$ as well.

``` r
convert_tabular(
  crosstab,
  `Miles per galleon` ~ Transmission,
  measures = 'n.primary'
)
#> Key: <Miles per galleon>
#>    Miles per galleon Automatic Manual Total
#>               <fctr>     <int>  <int> <int>
#> 1:             <20.0        15     NA    18
#> 2:         20.0-24.9         4      4     8
#> 3:             25.0+         0      6     6
#> 4:             20.0+         4     10    14
#> 5:           Unknown         0      0     0
#> 6:             Total        19     13    32
```

So, we need to conduct secondary suppression. The relationships of a
table are available from its `MappingTable` object in the `nullspace`
binding.

``` r
str(CrossMpgTransmission$nullspace)
#>  num [1:10, 1:18] 1 0 0 0 1 0 0 0 0 0 ...
```

Each row of this matrix describes a different relationship within the
table. We can use this information to conduct secondary suppression
using the `suppress_secondary()` function.

``` r
crosstab$suppress2ary <- suppress_secondary(
  N = crosstab$n,
  suppress = crosstab$suppress,
  # NB crosstab must have same order as CrossMpgTransmission$mtab 
  nullspace = CrossMpgTransmission$nullspace
)
```

When we look at the table following this suppression, there is no way to
back-calculate the primary-suppressed cell exactly.

``` r
crosstab$n.secondary <- crosstab$n
crosstab$n.secondary[crosstab$suppress2ary] <- NA

convert_tabular(
  crosstab,
  `Miles per galleon` ~ Transmission,
  measures = 'n.secondary'
)
#> Key: <Miles per galleon>
#>    Miles per galleon Automatic Manual Total
#>               <fctr>     <int>  <int> <int>
#> 1:             <20.0        NA     NA    18
#> 2:         20.0-24.9        NA     NA     8
#> 3:             25.0+         0      6     6
#> 4:             20.0+        NA     NA    14
#> 5:           Unknown         0      0     0
#> 6:             Total        19     13    32
```

Unfortunately, we have also lost a lot of information in our table. This
highlights the need for care in designing tables so that they have
sufficient data and small cells are rare.

## Thanks

The authors would like to acknowledge the [NSW Ministry of
Health](https://www.health.nsw.gov.au/), which supported the initial
development of this package while the authors (Ian Powell, Hafiz
Khusyairi, and Peter Moritz) were employed there.
