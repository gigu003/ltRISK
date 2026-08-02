# Estimate disease risk and its confidence interval

Estimate disease risk using population-based cancer registry data,
including age-specific cancer incidence, cancer mortality, all-cause
mortality, and corresponding population data. Available methods include
adjusting for multiple primary cancers (AMP), the DevCan method
recommended by Surveillance, Epidemiology, and End Results (SEER), and
the Wun method.

## Usage

``` r
calc_ltr(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_start = min(ages),
  age_end = Inf,
  age_combine = c("cross", "pairwise"),
  alpha = 0.05,
  risk_func = "devcan",
  ci_method = "gamma",
  variance_method = "auto",
  multiplier = 100,
  digits = 6,
  return_variance = FALSE,
  ...
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

- age_start:

  Starting age(s) used to calculate risk over one or multiple age
  ranges.

- age_end:

  Ending age(s) used to calculate risk over one or multiple age ranges.

- age_combine:

  How `age_start` and `age_end` are combined. `"cross"` (default)
  returns every valid combination. `"pairwise"` pairs elements
  positionally, allowing scalar recycling.

- alpha:

  Alpha level specifying the confidence level for estimating the risk
  confidence interval.

- risk_func:

  Function used to estimate cancer risk. Options are "devcan", "amp",
  "wun" or "cumulative", with "devcan" as the default. The legacy alias
  "cumu" is also accepted.

- ci_method:

  Method used to estimate confidence intervals for the risk. Options are
  "gamma", "delta", or "none", with "gamma" as the default. Use "none"
  to skip variance and confidence-interval calculations.

- variance_method:

  Method used to estimate the risk variance. "auto" uses an analytic
  gradient for DevCan, AMP, Wun, and cumulative-risk models and
  otherwise falls back to "finite_difference". Analytic gradients
  support constant and PMAJ rates where applicable. Wun models with
  surgery correction use finite differences. "analytic" requires an
  implemented analytic gradient. Default is "auto".

- multiplier:

  Numeric value to scale the risk and confidence interval. Default is
  100, which reports risk as a percentage.

- digits:

  Integer indicating the number of decimal places to round the results.
  Default is 6.

- return_variance:

  Logical. If `TRUE`, also return the estimated variance and standard
  error of the risk on the same scale as `multiplier`. These values are
  not rounded by `digits`, so they can be used for downstream
  statistical tests. Default is `FALSE`.

- ...:

  Additional arguments passed to the risk estimation function, depending
  on the value of risk_func. If risk_func = "devcan", see ?devcan for
  available parameters.

  Input counts must be non-negative, `pys` must be positive, and cancer
  deaths must not exceed all-cause deaths. Variances treat cancer
  incidence, cancer deaths, and other deaths as independent Poisson
  count components and treat person-years as fixed. Gamma intervals
  return zero limits for an age range with zero estimated risk and no
  positive finite local perturbation.

## Value

A data frame containing the following variables:

- `start` — starting age of the age range for the risk.

- `end` — ending age of the age range for the risk.

- `risk` — estimated risk value.

- `lower` — lower bound of the confidence interval.

- `upper` — upper bound of the confidence interval.

- `variance` — estimated variance of the risk, returned only when
  `return_variance = TRUE`.

- `se` — estimated standard error of the risk, returned only when
  `return_variance = TRUE`.

## References

Sasieni PD, Shelton J, Ormiston-Smith N, Thomson CS, Silcocks PB. What
is the lifetime risk of developing cancer?: the effect of adjusting for
multiple primaries. *Br J Cancer*, 2011;105:460–465. DOI:
10.1038/bjc.2011.250

Fay MP, Pfeiffer R, Cronin KA, Le C, Feuer EJ. (2003). *Age-conditional*
*probabilities of developing cancer*. *Statistics in Medicine*,
22(11):1837-1848. DOI: 10.1002/sim.1428.

Fay M P. *Estimating age conditional probability of developing disease*
*from surveillance data\[J\]*. Population Health Metrics, 2004, 2(1): 6.

## Examples

``` r
breast <- seer_example_data[seer_example_data$site=="Breast",]
calc_ltr(ages = breast$ages, cancer = breast$cancer,
         cancer_death = breast$cancer_death, death = breast$death,
         pys = breast$pys, maj_method = "constant",
         age_start = c(0, 30, 50, 70), age_end = c(30, 50, 70, Inf),
         digits = 4)
#>    start end    risk   lower   upper
#> 1      0  30  0.0470  0.0424  0.0519
#> 2      0  50  1.8995  1.8708  1.9286
#> 3      0  70  7.7861  7.7130  7.8598
#> 4      0 Inf 13.3198 13.2170 13.4235
#> 5     30  50  1.8817  1.8529  1.9108
#> 6     30  70  7.8609  7.7868  7.9355
#> 7     30 Inf 13.4816 13.3773 13.5868
#> 8     50  70  6.2505  6.1793  6.3224
#> 9     50 Inf 12.1264 12.0217 12.2320
#> 10    70 Inf  7.3149  7.2202  7.4109
```
