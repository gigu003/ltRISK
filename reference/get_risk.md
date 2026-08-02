# Calculate the risk value for a specific age range

Calculate the risk value for a specific age range

## Usage

``` r
get_risk(object, age_start, age_end, age_combine = c("cross", "pairwise"))

# S3 method for class 'devcan'
get_risk(object, age_start, age_end, age_combine = c("cross", "pairwise"))

# S3 method for class 'amp'
get_risk(object, age_start, age_end, age_combine = c("cross", "pairwise"))

# S3 method for class 'wun'
get_risk(object, age_start, age_end, age_combine = c("cross", "pairwise"))

# S3 method for class 'cumu'
get_risk(object, age_start, age_end, age_combine = c("cross", "pairwise"))
```

## Arguments

- object:

  Returned object from a risk model calculated using the devcan, amp, or
  wun function.

- age_start:

  Starting age(s) used to calculate risk over one or multiple age
  ranges.

- age_end:

  Ending age(s) used to calculate risk over one or multiple age ranges.

- age_combine:

  How age vectors are combined: `"cross"` or `"pairwise"`.

## Value

Risks according to the age ranges.
