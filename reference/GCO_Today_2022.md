# GCO_Today_2022

A data frame containing the number of cancer cases, cancer deaths
(cancer mortality), all-cause deaths, and population counts stratified
by age groups, sex, and cancer sites in 20 regions worldwide and world
total. The cancer cases and cancer deaths are obtained from the Global
Cancer Observatory Today, while the population counts and all-cause
deaths are sourced from the World Population Prospects 2022.

## Usage

``` r
GCO_Today_2022
```

## Format

A data frame with 40,824 rows and 8 variables:

- site_code:

  `integer`: Cancers include the code of cancer sites.

- icd10:

  `character`: ICD-10 codes corresponding to cancer sites.

- site_abbr:

  `character`: Cancer site abbreviated description.

- ages:

  `integer`: Starting age of each age group.

- cancer:

  `numeric`: Number of (registered) cancer cases.

- cancer_death:

  `numeric`: Number of cancer deaths (cancer mortality).

- death:

  `numeric`: Number of deaths (all-cause mortality).

- pys:

  `numeric`: The size of the mid-year population.

## Source

The cancer cases and cancer deaths are obtained from the Global Cancer
Observatory Today <https://gco.iarc.fr/today/en>, while the population
counts and all-cause deaths are sourced from the World Population
Prospects 2022 <https://population.un.org/wpp/>.

## Details

The data is collected from two main sources: cancer cases and deaths
from the Global Cancer Observatory Today, and population and all-cause
deaths from the World Population Prospects 2022. The data covers 20
world regions and includes estimates for different age groups and sexes.

## References

Global Cancer Observatory Today: <https://gco.iarc.fr/today/en> World
Population Prospects 2022: <https://population.un.org/wpp/>

## Examples

``` r
# Load the dataset
data(GCO_Today_2022)

# Display the first few rows
head(GCO_Today_2022)
#> # A tibble: 6 × 8
#>   site_code icd10  site_abbr         ages cancer cancer_death   death        pys
#>       <int> <chr>  <chr>            <dbl>  <dbl>        <dbl>   <dbl>      <dbl>
#> 1         1 C00-06 Lip, oral cavity     0    329          114 4715187 654355571 
#> 2         1 C00-06 Lip, oral cavity     5    319          107  709923 682607098.
#> 3         1 C00-06 Lip, oral cavity    10    455          145  457714 664762157 
#> 4         1 C00-06 Lip, oral cavity    15    648          261  683239 627243354 
#> 5         1 C00-06 Lip, oral cavity    20   1023          683  895411 601601216.
#> 6         1 C00-06 Lip, oral cavity    25   3762         1527  981907 589721628.
```
