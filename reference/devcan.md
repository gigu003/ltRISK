# Compute probabilities used for estimation of lifetime risk

Compute probabilities used for estimation of lifetime risk

## Usage

``` r
devcan(
  ages,
  cancer,
  death,
  cancer_death,
  pys,
  type = "developing",
  no_other_death = FALSE,
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

- death:

  Number of all deaths (all causes combined) in each age interval.

- cancer_death:

  Number of deaths due to cancer in each age interval.

- pys:

  Person-years at risk corresponding to each age interval.

- type:

  Characters "developing" or "dying" indicate estimate the probability
  of developing cancer or dying from it.

- no_other_death:

  Logical. If `TRUE`, assumes absence of other causes of death (i.e.,
  non-cancer mortality = 0). Default = FALSE.

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

Contribution of risk of developing cancer or dying from it in each age
group.

## References

Fay MP, Pfeiffer R, Cronin KA, Le C, Feuer EJ. (2003). *Age-conditional*
*probabilities of developing cancer*. *Statistics in Medicine*,
22(11):1837-1848. DOI: 10.1002/sim.1428.

Fay M P. *Estimating age conditional probability of developing disease*
*from surveillance data\[J\]*. Population Health Metrics, 2004, 2(1): 6.
