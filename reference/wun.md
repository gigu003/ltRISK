# Compute probabilities used for estimation of lifetime risk

Compute probabilities used for estimation of lifetime risk

## Usage

``` r
wun(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  correct_for_surgery = FALSE,
  H = NULL,
  Ch = NULL,
  type = "developing",
  cohort_size = 1,
  last_age_widths = Inf
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

- correct_for_surgery:

  Logical value, whether correct for factors.

- H:

  Numeric vector of the number of surgeries (e.g., hysterectomies) in
  each interval. Required if `correct_for_surgery = TRUE`. Corresponds
  to H_x in the paper, representing the total number of hysterectomies
  performed in the interval.

- Ch:

  Numeric vector of the number of new cancer cases treated by the
  surgery in each interval. Required if `correct_for_surgery = TRUE`.
  Corresponds to Ch_x in the paper, representing the number of new
  cancer cases treated by a hysterectomy.

- type:

  Characters "developing" or "dying" indicate estimate the probability
  of developing cancer or dying from it.

- cohort_size:

  Size of the cohort.

- last_age_widths:

  Width of the last age group (default = Inf).

## Value

A list of class "wun" containing:

- `age`: The input ages.

- `contrib_a`: Contributions to new cancers (a_x).

- `condi_p`: Conditional probabilities.

- `l_cf`: Cancer-free (and surgery-free if applicable) alive at start.

- `d`: Non-cancer deaths among cancer-free.

- `s`: Non-cancer surgeries (if `correct_for_surgery = TRUE`).

## References

Wun, L. M., Merrill, R. M., & Feuer, E. J. (1998). Estimating lifetime
and age-conditional probabilities of developing cancer. Lifetime Data
Analysis, 4(2), 169-186.
