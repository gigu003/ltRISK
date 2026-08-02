# Compute probabilities used for estimation of lifetime risk

Estimates the lifetime and age-conditional probabilities of developing
cancer, adjusted for multiple primary cancers, using a competing risks
framework based on the method described in Sasieni et al. (2011).

## Usage

``` r
amp(
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

Contribution of risk of developing cancer or dying from it in each age
group.

## References

Sasieni PD, Shelton J, Ormiston-Smith N, Thomson CS, Silcocks PB. What
is the lifetime risk of developing cancer?: the effect of adjusting for
multiple primaries. *Br J Cancer*, 2011;105:460–465. DOI:
10.1038/bjc.2011.250
