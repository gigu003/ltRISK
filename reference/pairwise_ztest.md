# Pairwise Z tests for risk differences among groups

`pairwise_ztest()` performs pairwise large-sample Z tests for risk
estimates across two or more independent groups. The input should be a
grouped result returned by
[`calc_ltr_df()`](https://github.com/gigu003/ltRISK/reference/calc_ltr_df.md)
with `return_variance = TRUE`.

## Usage

``` r
pairwise_ztest(
  x,
  group,
  ref = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  p_adjust_method = "holm",
  digits = 6
)
```

## Arguments

- x:

  A grouped data frame returned by
  [`calc_ltr_df()`](https://github.com/gigu003/ltRISK/reference/calc_ltr_df.md)
  with `return_variance = TRUE`.

- group:

  Character string giving the grouping column in `x`.

- ref:

  Optional reference group value. If `NULL`, all pairwise comparisons
  are performed. If supplied, each non-reference group is compared
  against `ref`.

- alpha:

  Alpha level for the confidence interval of each difference.

- alternative:

  Alternative hypothesis. Options are `"two.sided"`, `"less"`, or
  `"greater"`. The alternative is applied to `risk_compare - risk_ref`.

- p_adjust_method:

  Method passed to
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) for
  multiplicity adjustment. Use `"none"` to return unadjusted P values.

- digits:

  Integer indicating the number of decimal places used to round the
  returned estimates and test statistics.

## Value

A data frame containing group labels, matched age ranges, risks, risk
difference, standard error of the difference, Z statistic, raw P value,
adjusted P value, and confidence interval for the difference.

## Examples

``` r
data("seer_example_data")
all_sites <- seer_example_data[
  seer_example_data$site == "All" & seer_example_data$sex %in% c(1, 2),
]
risks <- calc_ltr_df(
  all_sites,
  by = "sex",
  ages = ages,
  cancer = cancer,
  cancer_death = cancer_death,
  death = death,
  pys = pys,
  maj_method = "constant",
  return_variance = TRUE
)
pairwise_ztest(risks, group = "sex")
#>   group_compare group_ref start end risk_compare risk_ref difference       se
#> 1             2         1     0 Inf     38.97341 39.48811  -0.514699 0.061274
#>           z            p   p_adjusted     lower     upper
#> 1 -8.399974 4.465792e-17 4.465792e-17 -0.634794 -0.394604
```
