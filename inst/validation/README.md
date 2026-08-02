# Statistical validation

The package test suite contains a direct reproduction of Fay et al. (2003)
Table II using the paper's Table I breast-cancer counts. See
`tests/testthat/fixtures/README.md` for provenance.

`simulate-ci-coverage.R` performs a seeded parametric Poisson simulation based
on the same data. It covers DevCan, AMP, Wun, and cumulative risk; developing
and dying outcomes; constant and PMAJ rates where supported; regular and sparse
information settings; and gamma and delta confidence intervals. Counts and
person-years are scaled together so that information size changes while the
underlying rates remain fixed.

Run the full diagnostic from the package root:

```sh
Rscript inst/validation/simulate-ci-coverage.R 200
```

The optional second argument selects the output directory, and the optional
third argument is a regular expression used to select scenario names. For a
quick DevCan smoke run:

```sh
Rscript inst/validation/simulate-ci-coverage.R 2 /tmp/ltRISK-validation '^devcan__developing__constant__regular$'
```

The script writes `scenarios.csv`, replicate-level `coverage-raw.csv`,
`coverage-summary.csv`, `performance-summary.csv`, `method-comparison.csv`, and
`metadata.csv`. The summaries report coverage, interval width, bias, failure
rate, elapsed time, and the Gamma-to-Delta timing ratio. Generated results under
`inst/validation/results/` are ignored by Git; selected validation snapshots
may be copied elsewhere and versioned deliberately.

DevCan, AMP, and Wun include open-ended lifetime targets. Traditional
cumulative risk instead uses finite upper ages (85 and 70), because applying a
positive terminal rate over an infinite interval makes cumulative risk exactly
one and yields an uninformative zero-width interval.

This simulation is intentionally kept outside routine tests because coverage
assessment is stochastic and computationally expensive. It is a diagnostic,
not proof that nominal coverage holds for every rate structure or sparse-data
setting.