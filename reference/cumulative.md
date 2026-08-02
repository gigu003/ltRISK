# Calculate cumulative rate and risk.

This function computes age-specific rates and cumulative risk
contributions across age intervals based on cancer incidence or cancer
mortality counts. It converts interval counts into rates using
person-years, and then calculates the contribution of each age group to
the cumulative risk using \\1 - \exp(-r_x \cdot n_x)\\, where \\r_x\\ is
the age-specific rate and \\n_x\\ is the width of the age interval.

## Usage

``` r
cumulative(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  last_age_widths = Inf,
  type = "developing",
  maj_method = "pmaj",
  pmaj_sub_interval = 0.5
)
```

## Arguments

- ages:

  Starting ages of each age interval (e.g., 0, 5, 10, ..., 85).

- cancer:

  Number of cancer diagnoses in each age interval. For AMP, this may
  include multiple primary cancers.

- cancer_death:

  Number of deaths due to cancer in each age interval.

- death:

  Number of all deaths (all causes combined) in each age interval.

- pys:

  Person-years at risk corresponding to each age interval.

- last_age_widths:

  Width of the last age group (default = Inf).

- type:

  Characters "developing" or "dying" indicate estimate the probability
  of developing cancer or dying from it.

- maj_method:

  Character string. The method to use for smoothing the rates: "pmaj"
  for Piece-wise Mid-Age Group Joinpoint, "constant" for simple
  piece-wise constant. Default is "pmaj". Note: To approximate the exact
  MAJ (Mid-Age Group Joinpoint) method, use "pmaj" with a small value
  for `pmaj_sub_interval` (e.g., 0.01), as MAJ requires numerical
  integration for exact computation but can be closely approximated this
  way.

- pmaj_sub_interval:

  Numeric. Sub-interval size for pmaj (default = 0.5 years).

## Value

A list of class `"cumu"` containing:

- ages:

  Vector of starting ages.

- widths:

  Vector of widths for each age interval.

- rate:

  Age-specific rates, computed as `count / pys`.

## See also

[`get_risk.cumu`](https://github.com/gigu003/ltRISK/reference/get_risk.md)
for obtaining cumulative risks across arbitrary age ranges.
