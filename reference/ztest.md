# Test the difference in risk between two groups

`ztest()` compares risk estimates from two independent groups using a
large-sample Z test. The inputs should be results returned by
[`calc_ltr()`](https://github.com/gigu003/ltRISK/reference/calc_ltr.md)
or
[`calc_ltr_df()`](https://github.com/gigu003/ltRISK/reference/calc_ltr_df.md)
with `return_variance = TRUE`.

## Usage

``` r
ztest(
  x,
  y = NULL,
  group = NULL,
  ref = NULL,
  compare = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  digits = 6
)

# S3 method for class 'data.frame'
ztest(
  x,
  y = NULL,
  group = NULL,
  ref = NULL,
  compare = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  digits = 6
)
```

## Arguments

- x:

  A data frame returned by
  [`calc_ltr()`](https://github.com/gigu003/ltRISK/reference/calc_ltr.md)
  with `return_variance = TRUE`, or a grouped result returned by
  [`calc_ltr_df()`](https://github.com/gigu003/ltRISK/reference/calc_ltr_df.md).

- y:

  Optional. A second data frame returned by
  [`calc_ltr()`](https://github.com/gigu003/ltRISK/reference/calc_ltr.md)
  with `return_variance = TRUE`. If `NULL`, `group` must be supplied and
  the two groups are selected from `x`.

- group:

  Optional character string giving the grouping column in `x`. Used when
  `y = NULL`.

- ref:

  Reference group value when `group` is supplied.

- compare:

  Comparison group value when `group` is supplied. The reported
  difference is `risk_compare - risk_ref`.

- alpha:

  Alpha level for the confidence interval of the difference.

- alternative:

  Alternative hypothesis. Options are `"two.sided"`, `"less"`, or
  `"greater"`. For grouped input, the alternative is applied to
  `risk_compare - risk_ref`; for two-input usage it is applied to
  `risk_x - risk_y`.

- digits:

  Integer indicating the number of decimal places used to round the
  returned estimates and test statistics.

## Value

A data frame containing group labels, matched age ranges, risks, risk
difference, standard error of the difference, Z statistic, P value, and
confidence interval for the difference.

## Examples

``` r
data("seer_example_data")
breast <- seer_example_data[seer_example_data$site == "Breast", ]
female <- breast[breast$sex == 2, ]
male <- breast[breast$sex == 1, ]
if (nrow(female) > 0 && nrow(male) > 0) {
  risk_female <- calc_ltr(
    female$ages, female$cancer, female$cancer_death, female$death,
    female$pys, maj_method = "constant", return_variance = TRUE
  )
  risk_male <- calc_ltr(
    male$ages, male$cancer, male$cancer_death, male$death,
    male$pys, maj_method = "constant", return_variance = TRUE
  )
  ztest(risk_male, risk_female)
}
```
