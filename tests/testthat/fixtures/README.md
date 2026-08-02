# External validation fixtures

`fay2003-table-ii.csv` transcribes the female breast cancer values in Table II
of Fay et al. (2003), DOI `10.1002/sim.1428`. The input counts are Table I of
that paper and are stored in `seer_example_data` with `site == "Breast"`.

Values are percentages reported to four decimal places. Regression tests compare
at that published precision rather than treating package-generated high-precision
values as an external reference.

`seer-devcan-all-gamma.csv` is the tidy transcription of
`data-raw/seer_result.csv`, exported from NCI DevCan 6.7.5 for SEER 21 incidence
and mortality, 2019–2022 with 2020 excluded. It corresponds to
`seer_example_data` with `site == "All"` and `sex == 0`, developing all-site
cancer, PMAJ rates, and Gamma confidence intervals. It contains all 190 valid
combinations of starting ages 0, 5, ..., 90 and later ending ages 5, 10, ...,
90, or the open 90+ interval.

DevCan displays point estimates to two decimal places and confidence limits to
four decimal places. Tests therefore compare point estimates at two decimals
and allow 0.00021 percentage points for confidence limits. This accommodates
the observed final-digit differences caused by hidden intermediate precision
and display rounding while remaining much smaller than the displayed 0.01%
point-estimate unit. Recreate the tidy fixture with:

```sh
Rscript data-raw/prepare-seer-result.R
```