# ltRISK 0.1.0

* Established `calc_ltr()` and `calc_ltr_df()` as the unified vector and
  data-frame interfaces for the initial public release. Pre-release experimental
  `ltr`, `cumrisk`, `cumrate`, and `estimate` interfaces were removed before
  publication and therefore do not require a deprecation layer.
* Unified zero-count variance handling between analytic and finite-difference
  confidence intervals. Delta intervals consistently use the documented 0.5
  correction, while Gamma intervals retain exact zero variance.
* Added finite final-age interval support to AMP and cumulative-risk models,
  including PMAJ grids and analytic variances.
* Fixed cumulative-risk extraction at the terminal boundary of a finite final
  age interval.
* Improved numerical handling of zero rates in open intervals and added
  explicit checks for invalid variance results.
* Added `age_combine = "pairwise"` to calculate positional age intervals while
  retaining the existing cross-combination behavior by default.
* Added early validation for strictly increasing age groups, age endpoints,
  `digits`, `multiplier`, and logical output options.
* Improved diagnostics for unavailable age boundaries under constant-rate
  models.
* Public risk models now apply consistent validation to registry counts,
  person-years, age groups, model type, and rate inputs.

* `calc_ltr()` now validates core registry inputs, handles zero-risk gamma
  intervals, and calculates numerical-delta variances with matrix operations.
* `calc_ltr()` reuses batched age-range calculations and deduplicates local
  maxima when constructing gamma upper confidence limits, substantially
  improving confidence-interval speed while preserving existing estimates.
* `pmaj()` now retains exact age boundaries when the requested sub-interval
  width does not evenly divide an age interval.
* `calc_ltr(ci_method = "none")` now provides a fast point-estimate-only path.
* Added `variance_method` with analytic gradients for constant-rate cumulative
  risk and DevCan models; unsupported models safely fall back to finite
  differences when `variance_method = "auto"`.
* PMAJ grids now retain their interpolation design matrix internally, allowing
  cumulative-risk and DevCan gradients to be mapped back to the original
  registry age groups without finite-difference model refits.
* PMAJ grid geometry is cached by age structure and sub-interval width, avoiding
  repeated interpolation setup during gamma confidence-limit searches.
* DevCan gamma candidate risks now use an equivalent vectorized evaluator while
  retaining exact enumeration of all local perturbations.
* AMP now uses analytic gradients for both constant and PMAJ rates, including
  developing and dying risks and conditional age ranges.
* AMP gamma candidate risks now use a vectorized evaluator with exact local
  candidate enumeration and cached PMAJ geometry.
* Cumulative-risk gamma candidate searches now use a direct vectorized
  evaluator instead of rebuilding the public model for every local count
  perturbation, substantially reducing PMAJ Gamma runtime.
* `calc_ltr_df()` can now calculate independent cancer, sex, year, or other
  groups in parallel with the cross-platform `parallel` and `workers` options.
  Group tasks transfer only required vectors, use load-balanced scheduling, and
  can reuse a caller-managed PSOCK cluster across consecutive calculations.
* Added `clear_ltr_cache()` and `ltr_cache_info()` for explicit memory-cache
  management. Grouped calculations can use `cache = "clear"` to bound cache
  growth in long-running batch jobs, including on reused PSOCK workers.
* Added `cache = "none"` to `calc_ltr_df()` to discard memoised entries after
  every group, limiting peak cache growth in large serial and parallel jobs.
* Risk results now retain their calculation precision, so
  `format_risk_ci(digits = NULL)` automatically inherits the `digits` value used
  by `calc_ltr()` or `calc_ltr_df()` while still allowing an explicit override.
* `get_risk()` now extracts AMP risks for multiple age ranges with cumulative
  contributions instead of nested age-band loops.
* `trend_test()` and `ztest()` now reject duplicate, singular, non-finite, and
  zero-standard-error inputs with explicit diagnostics; the obsolete `ltr`
  method has been removed.
* `wun()` now handles zero-rate and single-interval data safely, validates
  surgery corrections, and diagnoses unidentified open-ended intervals.
* Wun finite-difference and gamma candidate calculations now use a direct
  pairwise risk evaluator, reducing repeated dispatch and range-construction
  overhead while preserving the public model estimates.
* Wun developing and dying risks now use analytic gradients by default when
  surgery correction is disabled. Surgery-corrected calculations continue to
  fall back to finite differences because uncertainty in surgery counts is not
  specified by the current API.
* Added `inst/benchmarks/benchmark-ci.R` for reproducible cold-cache timing of
  analytic and finite-difference confidence-interval paths.
* Added an external regression fixture that reproduces the DevCan risks and
  gamma and delta confidence intervals in Fay et al. (2003), Table II, at the
  published precision.
* Added a 190-interval external regression fixture reproducing the NCI DevCan
  6.7.5 SEER 21 all-sites developing-risk table and Gamma confidence limits.
* Gamma upper-limit searches now preserve the non-negative other-death count
  constraint and exclude non-finite boundary perturbations.
* Documented the independent-Poisson count model, fixed-denominator assumption,
  limitations of independent-group comparisons, and identifying assumptions.
  Added a seeded parametric coverage diagnostic under `inst/validation` with
  28 model, outcome, rate-shape, and information-density scenarios, plus
  structured coverage and Gamma-versus-Delta timing summaries.