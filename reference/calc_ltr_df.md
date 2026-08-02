# Estimate disease risk from a data frame

`calc_ltr_df()` is a data-frame interface to
[`calc_ltr()`](https://github.com/gigu003/ltRISK/reference/calc_ltr.md).
It lets users provide one data frame and specify the columns containing
age-specific cancer incidence, cancer mortality, all-cause mortality,
and person-years.

## Usage

``` r
calc_ltr_df(
  data,
  ages = "ages",
  cancer = "cancer",
  cancer_death = "cancer_death",
  death = "death",
  pys = "pys",
  by = NULL,
  parallel = FALSE,
  workers = NULL,
  cluster = NULL,
  cache = c("retain", "clear", "none"),
  ...
)
```

## Arguments

- data:

  A data frame containing the input variables.

- ages, cancer, cancer_death, death, pys:

  Column names in `data`. Each can be supplied as a character string, a
  bare column name, or a character variable.

- by:

  Optional character vector of grouping columns. When supplied,
  `calc_ltr_df()` calculates risks separately within each group and
  returns the grouping columns together with the risk estimates. If
  `data` is a
  [`dplyr::grouped_df`](https://dplyr.tidyverse.org/reference/grouped_df.html)
  and `by = NULL`, grouping variables are detected automatically.

- parallel:

  Logical. If `TRUE`, calculate independent groups in parallel using a
  cross-platform PSOCK cluster. Parallel execution is used only when
  there is more than one group and more than one worker. Default is
  `FALSE`.

- workers:

  Number of parallel worker processes. `NULL` chooses up to one fewer
  than the detected logical cores, capped by the number of groups.
  `workers = 1` always uses the serial path. Ignored when
  `parallel = FALSE`. Because each PSOCK worker is a separate R process,
  parallel execution is most useful for many computationally intensive
  groups (for example, AMP or DevCan gamma intervals) and uses
  additional memory.

- cluster:

  Optional PSOCK cluster created by
  [`parallel::makePSOCKcluster()`](https://rdrr.io/r/parallel/makeCluster.html).
  When supplied, it is reused and is not stopped by `calc_ltr_df()`.
  This is useful for several consecutive grouped calculations. `workers`
  is ignored when `cluster` is supplied.

- cache:

  Cache policy. `"retain"` (default) retains memoised results for later
  calls. `"clear"` clears caches after the complete calculation.
  `"none"` additionally clears caches after every group, limiting peak
  cache growth in long grouped jobs. With an external `cluster`,
  clearing also occurs on workers while leaving the cluster running.

- ...:

  Additional arguments passed to
  [`calc_ltr()`](https://github.com/gigu003/ltRISK/reference/calc_ltr.md),
  such as `risk_func`, `type`, `maj_method`, `ci_method`, `age_start`,
  and `age_end`.

## Value

A data frame returned by
[`calc_ltr()`](https://github.com/gigu003/ltRISK/reference/calc_ltr.md).

## Examples

``` r
# One population using the package's standard column names
breast <- seer_example_data[seer_example_data$site == "Breast", ]
calc_ltr_df(
  breast,
  maj_method = "constant",
  ci_method = "none",
  age_start = c(0, 30, 50, 70),
  age_end = c(30, 50, 70, Inf),
  age_combine = "pairwise",
  digits = 4
)
#>   start end   risk
#> 1     0  30 0.0470
#> 2    30  50 1.8817
#> 3    50  70 6.2505
#> 4    70 Inf 7.3149

# Calculate independent male and female risks in one call
all_by_sex <- seer_example_data[
  seer_example_data$site == "All" & seer_example_data$sex %in% c(1, 2),
]
calc_ltr_df(
  all_by_sex,
  by = "sex",
  maj_method = "constant",
  ci_method = "delta",
  age_start = 40,
  age_end = Inf,
  return_variance = TRUE
)
#>   sex start end     risk    lower    upper    variance         se
#> 1   1    40 Inf 40.45334 40.36281 40.54387 0.002133385 0.04618858
#> 2   2    40 Inf 38.33352 38.24782 38.41922 0.001911886 0.04372512

# Input columns can have other names
custom <- data.frame(
  age_group = breast$ages,
  cases = breast$cancer,
  cancer_deaths = breast$cancer_death,
  all_deaths = breast$death,
  population = breast$pys
)
calc_ltr_df(
  custom,
  ages = age_group,
  cancer = cases,
  cancer_death = "cancer_deaths",
  death = all_deaths,
  pys = population,
  ci_method = "none",
  maj_method = "constant"
)
#>   start end     risk
#> 1     0 Inf 13.31979

if (FALSE) { # \dontrun{
# Parallel grouped calculation and reusable cluster
calc_ltr_df(
  seer_example_data,
  by = c("site", "sex"),
  ci_method = "delta",
  parallel = TRUE,
  workers = 2
)

cl <- parallel::makePSOCKcluster(2)
calc_ltr_df(
  seer_example_data,
  by = c("site", "sex"),
  ci_method = "none",
  cluster = cl
)
parallel::stopCluster(cl)
} # }
```
