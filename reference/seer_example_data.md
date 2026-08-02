# Example Cancer Data from SEER

Example Cancer Data from SEER

## Usage

``` r
seer_example_data
```

## Format

A data frame with 40 rows and 7 variables:

- site:

  Character. Breast for breast cancer, All for all sites.

- period:

  Character. Time period corresponding to data.

- sex:

  Integer. Sex indicator (0 = Both, 1 = Male, 2 = Female).

- ages:

  Integer. The starting age of the interval (e.g., 0, 1, 5...).

- cancer:

  Integer. Count of first primary incident cancer cases.

- cancer_death:

  Integer. Deaths specifically attributed to this cancer site.

- death:

  Integer. Total deaths from all causes in the age interval.

- pys:

  Numeric. Person-years at risk, typically the mid-year population.

## Source

[doi:10.1002/sim.1428](https://doi.org/10.1002/sim.1428)

## References

Fay MP, Pfeiffer R, Cronin KA, Le C, Feuer EJ. (2003). Age-conditional
probabilities of developing cancer. *Statistics in Medicine*, 22(11),
1837–1848.

National Cancer Institute. DevCan 6.7.5: SEER 21 Incidence and
Mortality, 2019–2022 (2020 Excluded).

## Examples

``` r
data(seer_example_data)
# View the structure of the Fay et al. example data
head(seer_example_data)
#> # A tibble: 6 × 8
#>   site   period      sex  ages cancer death cancer_death     pys
#>   <chr>  <chr>     <dbl> <dbl>  <dbl> <dbl>        <dbl>   <dbl>
#> 1 Breast 1996-1998     2     0      0  5893            0 4052953
#> 2 Breast 1996-1998     2     5      0   561            0 4032790
#> 3 Breast 1996-1998     2    10      1   628            1 3784789
#> 4 Breast 1996-1998     2    15      9  1367            0 3810986
#> 5 Breast 1996-1998     2    20     43  1547            6 3675646
#> 6 Breast 1996-1998     2    25    335  2064           35 4138795
```
