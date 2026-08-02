# Manage ltRISK in-memory caches

`clear_ltr_cache()` removes memoised risk-model results and PMAJ grid
geometries from the current R process. It can also clear the
corresponding caches on an existing PSOCK cluster. `ltr_cache_info()`
reports the number of cached entries in the current process.

## Usage

``` r
clear_ltr_cache(cluster = NULL)

ltr_cache_info()
```

## Arguments

- cluster:

  Optional cluster created by
  [`parallel::makePSOCKcluster()`](https://rdrr.io/r/parallel/makeCluster.html).
  When supplied, caches are also cleared on every worker. The cluster
  remains running.

## Value

`clear_ltr_cache()` invisibly returns `NULL`. `ltr_cache_info()` returns
a data frame with cache names and entry counts.
