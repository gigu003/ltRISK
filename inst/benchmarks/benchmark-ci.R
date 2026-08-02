# Reproducible confidence-interval benchmark for ltRISK.
# Run from the package root with:
# Rscript inst/benchmarks/benchmark-ci.R

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Install devtools to run this source-package benchmark.")
}
devtools::load_all(quiet = TRUE)

d <- subset(seer_example_data, site == "All" & sex == 0)
base_args <- list(
  ages = d$ages,
  cancer = d$cancer,
  cancer_death = d$cancer_death,
  death = d$death,
  pys = d$pys,
  age_start = c(0, 30, 50, 70),
  age_end = c(30, 50, 70, Inf),
  age_combine = "pairwise",
  digits = 8
)

time_call <- function(args, iterations = 10L) {
  do.call(calc_ltr, args)
  clear_ltr_cache()
  elapsed <- replicate(
    iterations,
    {
      clear_ltr_cache()
      system.time(do.call(calc_ltr, args))[["elapsed"]]
    }
  )
  c(median = median(elapsed), min = min(elapsed), max = max(elapsed))
}

scenarios <- list(
  wun_delta_analytic = c(
    base_args,
    list(risk_func = "wun", ci_method = "delta", variance_method = "analytic")
  ),
  wun_delta_finite = c(
    base_args,
    list(
      risk_func = "wun",
      ci_method = "delta",
      variance_method = "finite_difference"
    )
  ),
  wun_gamma_analytic = c(
    base_args,
    list(risk_func = "wun", ci_method = "gamma", variance_method = "analytic")
  ),
  wun_gamma_finite = c(
    base_args,
    list(
      risk_func = "wun",
      ci_method = "gamma",
      variance_method = "finite_difference"
    )
  ),
  devcan_gamma = c(
    base_args,
    list(
      risk_func = "devcan",
      ci_method = "gamma",
      variance_method = "analytic",
      maj_method = "constant"
    )
  ),
  amp_gamma = c(
    base_args,
    list(
      risk_func = "amp",
      ci_method = "gamma",
      variance_method = "analytic",
      maj_method = "constant"
    )
  )
)

timings <- t(vapply(scenarios, time_call, numeric(3)))
result <- data.frame(
  scenario = rownames(timings),
  median_seconds = timings[, "median"],
  min_seconds = timings[, "min"],
  max_seconds = timings[, "max"],
  row.names = NULL
)
print(result, row.names = FALSE)
