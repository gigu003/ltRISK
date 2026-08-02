# Parametric coverage diagnostic for ltRISK confidence intervals.
# Run from the package root with:
# Rscript inst/validation/simulate-ci-coverage.R [nsim] [output_dir] [scenario_regex]

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Install devtools to run this source-package validation script.")
}
devtools::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
nsim <- if (length(args) >= 1L) as.integer(args[[1]]) else 200L
output_dir <- if (length(args) >= 2L) args[[2]] else "inst/validation/results"
scenario_regex <- if (length(args) >= 3L) args[[3]] else ".*"
if (is.na(nsim) || nsim < 1L) stop("nsim must be a positive integer")
if (!nzchar(output_dir)) stop("output_dir must not be empty")
invisible(tryCatch(grepl(scenario_regex, ""), error = function(e) {
  stop("scenario_regex is not a valid regular expression")
}))

seed <- 1428L
set.seed(seed)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

d <- subset(seer_example_data, site == "Breast")
other_death <- d$death - d$cancer_death
default_age_start <- c(0, 30, 50)
default_age_end <- c(Inf, Inf, 70)

# Scaling counts and person-years together preserves the underlying rates while
# changing information size. The sparse setting is intentionally challenging.
scenarios <- expand.grid(
  risk_func = c("devcan", "amp", "cumulative"),
  type = c("developing", "dying"),
  maj_method = c("constant", "pmaj"),
  density = c("regular", "sparse"),
  stringsAsFactors = FALSE
)
scenarios <- rbind(
  scenarios,
  expand.grid(
    risk_func = "wun",
    type = c("developing", "dying"),
    maj_method = "constant",
    density = c("regular", "sparse"),
    stringsAsFactors = FALSE
  )
)
scenarios$exposure_scale <- ifelse(scenarios$density == "regular", 1, 0.05)
scenarios$scenario <- with(
  scenarios,
  paste(risk_func, type, maj_method, density, sep = "__")
)
scenarios <- scenarios[grepl(scenario_regex, scenarios$scenario), ]
row.names(scenarios) <- NULL
if (!nrow(scenarios)) stop("scenario_regex selected no scenarios")

model_args <- function(scenario) {
  out <- list(type = scenario$type)
  if (scenario$risk_func != "wun") out$maj_method <- scenario$maj_method
  out
}

age_ranges <- function(scenario) {
  if (scenario$risk_func == "cumulative") {
    # Traditional cumulative risk requires a finite upper age. With an open
    # final interval and a positive terminal rate, risk is deterministically 1,
    # so an Inf endpoint is not an informative coverage target.
    return(list(start = c(0, 30, 50), end = c(85, 85, 70)))
  }
  list(start = default_age_start, end = default_age_end)
}

calculate <- function(scenario, cancer, cancer_death, death, pys, ci_method) {
  ranges <- age_ranges(scenario)
  do.call(
    calc_ltr,
    c(
      list(
        ages = d$ages,
        cancer = cancer,
        cancer_death = cancer_death,
        death = death,
        pys = pys,
        risk_func = scenario$risk_func,
        ci_method = ci_method,
        age_start = ranges$start,
        age_end = ranges$end,
        age_combine = "pairwise",
        multiplier = 1,
        digits = 15
      ),
      model_args(scenario)
    )
  )
}

truth_for <- function(scenario) {
  scale <- scenario$exposure_scale
  calculate(
    scenario,
    cancer = d$cancer * scale,
    cancer_death = d$cancer_death * scale,
    death = d$death * scale,
    pys = d$pys * scale,
    ci_method = "none"
  )$risk
}

simulate_counts <- function(scenario) {
  scale <- scenario$exposure_scale
  cancer <- stats::rpois(nrow(d), d$cancer * scale)
  cancer_death <- stats::rpois(nrow(d), d$cancer_death * scale)
  simulated_other <- stats::rpois(nrow(d), other_death * scale)
  list(
    cancer = cancer,
    cancer_death = cancer_death,
    death = cancer_death + simulated_other,
    pys = d$pys * scale
  )
}

run_method <- function(scenario, counts, truth, simulation, method) {
  started <- proc.time()[["elapsed"]]
  ranges <- age_ranges(scenario)
  tryCatch({
    result <- calculate(
      scenario,
      counts$cancer,
      counts$cancer_death,
      counts$death,
      counts$pys,
      method
    )
    data.frame(
      scenario = scenario$scenario,
      simulation = simulation,
      method = method,
      start = result$start,
      end = result$end,
      truth = truth,
      estimate = result$risk,
      lower = result$lower,
      upper = result$upper,
      covered = result$lower <= truth & truth <= result$upper,
      elapsed = proc.time()[["elapsed"]] - started,
      error = NA_character_,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      scenario = scenario$scenario,
      simulation = simulation,
      method = method,
      start = ranges$start,
      end = ranges$end,
      truth = truth,
      estimate = NA_real_, lower = NA_real_, upper = NA_real_,
      covered = NA, elapsed = proc.time()[["elapsed"]] - started,
      error = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

results <- vector("list", nrow(scenarios) * nsim * 2L)
position <- 0L
for (s in seq_len(nrow(scenarios))) {
  scenario <- scenarios[s, ]
  message(sprintf("[%d/%d] %s", s, nrow(scenarios), scenario$scenario))
  truth <- truth_for(scenario)
  for (i in seq_len(nsim)) {
    counts <- simulate_counts(scenario)
    for (method in c("gamma", "delta")) {
      position <- position + 1L
      results[[position]] <- run_method(scenario, counts, truth, i, method)
    }
  }
}
raw <- do.call(rbind, results)
raw$interval_width <- raw$upper - raw$lower
raw$bias <- raw$estimate - raw$truth
raw$failed <- !is.na(raw$error)

keys <- c("scenario", "method", "start", "end")
groups <- split(seq_len(nrow(raw)), raw[keys], drop = TRUE)
summary <- do.call(rbind, lapply(groups, function(idx) {
  x <- raw[idx, ]
  ok <- !x$failed
  data.frame(
    scenario = x$scenario[[1]], method = x$method[[1]],
    start = x$start[[1]], end = x$end[[1]], nsim = nsim,
    successful = sum(ok), failure_rate = mean(x$failed),
    coverage = if (any(ok)) mean(x$covered[ok]) else NA_real_,
    mean_width = if (any(ok)) mean(x$interval_width[ok]) else NA_real_,
    mean_bias = if (any(ok)) mean(x$bias[ok]) else NA_real_,
    mean_elapsed = mean(x$elapsed),
    stringsAsFactors = FALSE
  )
}))
row.names(summary) <- NULL

method_groups <- split(seq_len(nrow(summary)), summary$method)
performance <- do.call(rbind, lapply(method_groups, function(idx) {
  x <- summary[idx, ]
  data.frame(
    method = x$method[[1]],
    mean_elapsed = mean(x$mean_elapsed),
    median_elapsed = stats::median(x$mean_elapsed),
    max_elapsed = max(x$mean_elapsed),
    stringsAsFactors = FALSE
  )
}))
row.names(performance) <- NULL

comparison_keys <- c("scenario", "start", "end")
gamma_summary <- summary[summary$method == "gamma", ]
delta_summary <- summary[summary$method == "delta", ]
comparison <- merge(
  gamma_summary,
  delta_summary,
  by = comparison_keys,
  suffixes = c("_gamma", "_delta")
)
comparison$gamma_delta_time_ratio <-
  comparison$mean_elapsed_gamma / comparison$mean_elapsed_delta

metadata <- data.frame(
  seed = seed, nsim = nsim, scenarios = nrow(scenarios),
  completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  r_version = R.version.string, stringsAsFactors = FALSE
)
write.csv(scenarios, file.path(output_dir, "scenarios.csv"), row.names = FALSE)
write.csv(raw, file.path(output_dir, "coverage-raw.csv"), row.names = FALSE)
write.csv(summary, file.path(output_dir, "coverage-summary.csv"), row.names = FALSE)
write.csv(performance, file.path(output_dir, "performance-summary.csv"), row.names = FALSE)
write.csv(comparison, file.path(output_dir, "method-comparison.csv"), row.names = FALSE)
write.csv(metadata, file.path(output_dir, "metadata.csv"), row.names = FALSE)

print(summary, row.names = FALSE)
message("\nMean elapsed time by CI method:")
print(performance, row.names = FALSE)
message("Results written to: ", normalizePath(output_dir))
