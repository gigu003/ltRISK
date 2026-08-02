# Test linear trend in risk across ordered groups

`trend_test()` performs a weighted linear trend test for risk estimates
across three or more ordered groups. The input should usually be a
grouped result returned by
[`calc_ltr_df()`](https://github.com/gigu003/ltRISK/reference/calc_ltr_df.md)
with `return_variance = TRUE`.

## Usage

``` r
trend_test(
  x,
  group,
  score = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  digits = 6
)
```

## Arguments

- x:

  A data frame returned by
  [`calc_ltr_df()`](https://github.com/gigu003/ltRISK/reference/calc_ltr_df.md)
  with `return_variance = TRUE`.

- group:

  Character string giving the ordered grouping column in `x`.

- score:

  Optional numeric scores for the ordered groups. If `NULL`, numeric
  group values are used directly, factor levels are converted to
  `1, 2, ...`, and character groups are scored by order of first
  appearance with a warning. A named numeric vector can be used to
  explicitly map group values to scores.

- alpha:

  Alpha level for the confidence interval of the slope.

- alternative:

  Alternative hypothesis for the slope. Options are `"two.sided"`,
  `"less"`, or `"greater"`.

- digits:

  Integer indicating the number of decimal places used to round returned
  estimates and test statistics.

## Value

A data frame with one row per age range containing the number of groups,
trend slope, standard error, Z statistic, P value, and confidence
interval for the slope.
