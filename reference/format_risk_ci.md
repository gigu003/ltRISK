# Format risk estimates with 95% confidence intervals

`format_risk_ci()` combines risk estimates and their lower and upper
confidence interval bounds into a character vector suitable for tables.
When `risk` is a data frame, the formatted value is added as a new
column.

## Usage

``` r
format_risk_ci(
  risk,
  lower = NULL,
  upper = NULL,
  risk_col = "risk",
  lower_col = "lower",
  upper_col = "upper",
  name = "risk_95ci",
  digits = NULL,
  conf_level = 95,
  show_conf_level = FALSE,
  sep = ", ",
  na = NA_character_
)
```

## Arguments

- risk:

  Numeric vector of risk estimates, or a data frame containing risk
  estimates and confidence interval bounds.

- lower:

  Numeric vector of lower confidence interval bounds. When `risk` is a
  data frame, this can be the lower-bound column name.

- upper:

  Numeric vector of upper confidence interval bounds. When `risk` is a
  data frame, this can be the upper-bound column name.

- risk_col:

  Column name containing risk estimates when `risk` is a data frame.
  Default is `"risk"`.

- lower_col:

  Column name containing lower confidence interval bounds when `risk` is
  a data frame. Default is `"lower"`.

- upper_col:

  Column name containing upper confidence interval bounds when `risk` is
  a data frame. Default is `"upper"`.

- name:

  Column name for the formatted output when `risk` is a data frame.
  Default is `"risk_95ci"`.

- digits:

  Optional integer indicating the number of decimal places to display.
  If `NULL` and `risk` is a data frame returned by
  [`calc_ltr()`](https://github.com/gigu003/ltRISK/reference/calc_ltr.md)
  or
  [`calc_ltr_df()`](https://github.com/gigu003/ltRISK/reference/calc_ltr_df.md),
  the calculation's `digits` setting is inherited. Otherwise, values are
  converted with
  [`as.character()`](https://rdrr.io/r/base/character.html) without
  additional rounding.

- conf_level:

  Confidence level displayed in the formatted text. Default is `95`.

- show_conf_level:

  Logical. If `TRUE`, include the confidence level label such as
  `95% CI` before the interval bounds. Default is `FALSE`.

- sep:

  Separator between lower and upper confidence interval bounds. Default
  is `", "`.

- na:

  Character value used when any of `risk`, `lower`, or `upper` is
  missing for an observation. Default is `NA_character_`.

## Value

A character vector in the form `risk (lower, upper)`, or a data frame
with an added formatted column when `risk` is a data frame.

## Examples

``` r
format_risk_ci(risk = 12.3, lower = 10.1, upper = 14.5)
#> [1] "12.3 (10.1, 14.5)"
format_risk_ci(risk = 12.345, lower = 10.123, upper = 14.567, digits = 1)
#> [1] "12.3 (10.1, 14.6)"

breast <- seer_example_data[seer_example_data$site == "Breast", ]
res <- calc_ltr(
  ages = breast$ages, cancer = breast$cancer,
  cancer_death = breast$cancer_death, death = breast$death,
  pys = breast$pys, maj_method = "constant",
  ci_method = "delta", age_start = 0, age_end = Inf,
  digits = 2
)
res$risk_95ci <- format_risk_ci(res$risk, res$lower, res$upper, digits = 2)
format_risk_ci(res, digits = 2)
#>   start end  risk lower upper            risk_95ci
#> 1     0 Inf 13.32 13.22 13.42 13.32 (13.22, 13.42)
```
