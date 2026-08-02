#' Estimate disease risk from a data frame
#'
#' `calc_ltr_df()` is a data-frame interface to [calc_ltr()]. It lets users
#' provide one data frame and specify the columns containing age-specific cancer
#' incidence, cancer mortality, all-cause mortality, and person-years.
#'
#' @param data A data frame containing the input variables.
#' @param ages,cancer,cancer_death,death,pys Column names in `data`. Each can be
#'   supplied as a character string, a bare column name, or a character variable.
#' @param by Optional character vector of grouping columns. When supplied,
#'   `calc_ltr_df()` calculates risks separately within each group and returns the
#'   grouping columns together with the risk estimates. If `data` is a
#'   `dplyr::grouped_df` and `by = NULL`, grouping variables are detected
#'   automatically.
#' @param parallel Logical. If `TRUE`, calculate independent groups in parallel
#'   using a cross-platform PSOCK cluster. Parallel execution is used only when
#'   there is more than one group and more than one worker. Default is `FALSE`.
#' @param workers Number of parallel worker processes. `NULL` chooses up to one
#'   fewer than the detected logical cores, capped by the number of groups.
#'   `workers = 1` always uses the serial path. Ignored when `parallel = FALSE`.
#'   Because each PSOCK worker is a separate R process, parallel execution is
#'   most useful for many computationally intensive groups (for example, AMP or
#'   DevCan gamma intervals) and uses additional memory.
#' @param cluster Optional PSOCK cluster created by
#'   [parallel::makePSOCKcluster()]. When supplied, it is reused and is not
#'   stopped by `calc_ltr_df()`. This is useful for several consecutive grouped
#'   calculations. `workers` is ignored when `cluster` is supplied.
#' @param cache Cache policy. `"retain"` (default) retains memoised results for
#'   later calls. `"clear"` clears caches after the complete calculation.
#'   `"none"` additionally clears caches after every group, limiting peak cache
#'   growth in long grouped jobs. With an external `cluster`, clearing also
#'   occurs on workers while leaving the cluster running.
#' @param ... Additional arguments passed to [calc_ltr()], such as
#'   `risk_func`, `type`, `maj_method`, `ci_method`, `age_start`, and `age_end`.
#'
#' @returns A data frame returned by [calc_ltr()].
#' @export
#'
#' @examples
#' # One population using the package's standard column names
#' breast <- seer_example_data[seer_example_data$site == "Breast", ]
#' calc_ltr_df(
#'   breast,
#'   maj_method = "constant",
#'   ci_method = "none",
#'   age_start = c(0, 30, 50, 70),
#'   age_end = c(30, 50, 70, Inf),
#'   age_combine = "pairwise",
#'   digits = 4
#' )
#'
#' # Calculate independent male and female risks in one call
#' all_by_sex <- seer_example_data[
#'   seer_example_data$site == "All" & seer_example_data$sex %in% c(1, 2),
#' ]
#' calc_ltr_df(
#'   all_by_sex,
#'   by = "sex",
#'   maj_method = "constant",
#'   ci_method = "delta",
#'   age_start = 40,
#'   age_end = Inf,
#'   return_variance = TRUE
#' )
#'
#' # Input columns can have other names
#' custom <- data.frame(
#'   age_group = breast$ages,
#'   cases = breast$cancer,
#'   cancer_deaths = breast$cancer_death,
#'   all_deaths = breast$death,
#'   population = breast$pys
#' )
#' calc_ltr_df(
#'   custom,
#'   ages = age_group,
#'   cancer = cases,
#'   cancer_death = "cancer_deaths",
#'   death = all_deaths,
#'   pys = population,
#'   ci_method = "none",
#'   maj_method = "constant"
#' )
#'
#' \dontrun{
#' # Parallel grouped calculation and reusable cluster
#' calc_ltr_df(
#'   seer_example_data,
#'   by = c("site", "sex"),
#'   ci_method = "delta",
#'   parallel = TRUE,
#'   workers = 2
#' )
#'
#' cl <- parallel::makePSOCKcluster(2)
#' calc_ltr_df(
#'   seer_example_data,
#'   by = c("site", "sex"),
#'   ci_method = "none",
#'   cluster = cl
#' )
#' parallel::stopCluster(cl)
#' }
calc_ltr_df <- function(
  data,
  ages = "ages",
  cancer = "cancer",
  cancer_death = "cancer_death",
  death = "death",
  pys = "pys",
  by = NULL,
  parallel = FALSE,
  workers = NULL,
  cluster = NULL,
  cache = c("retain", "clear", "none"),
  ...
) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  by <- ltr_group_vars(data, by)
  parallel <- ltr_parallel_flag(parallel)
  cluster <- ltr_validate_cluster(cluster)
  cache <- match.arg(cache)
  if (cache != "retain") {
    on.exit(clear_ltr_cache(cluster), add = TRUE)
  }

  cols <- c(
    ages = if (missing(ages)) {
      "ages"
    } else {
      ltr_col_name(substitute(ages), parent.frame(), names(data))
    },
    cancer = if (missing(cancer)) {
      "cancer"
    } else {
      ltr_col_name(substitute(cancer), parent.frame(), names(data))
    },
    cancer_death = if (missing(cancer_death)) {
      "cancer_death"
    } else {
      ltr_col_name(substitute(cancer_death), parent.frame(), names(data))
    },
    death = if (missing(death)) {
      "death"
    } else {
      ltr_col_name(substitute(death), parent.frame(), names(data))
    },
    pys = if (missing(pys)) {
      "pys"
    } else {
      ltr_col_name(substitute(pys), parent.frame(), names(data))
    }
  )

  missing_cols <- setdiff(unname(cols), names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }

  missing_by <- setdiff(by, names(data))
  if (length(missing_by) > 0) {
    stop(
      "Missing grouping columns in data: ",
      paste(missing_by, collapse = ", ")
    )
  }

  if (length(by) > 0) {
    return(calc_ltr_df_by(
      data,
      cols = cols,
      by = by,
      parallel = parallel,
      workers = workers,
      cluster = cluster,
      cache = cache,
      ...
    ))
  }

  data <- data[order(data[[cols[["ages"]]]]), , drop = FALSE]
  calc_ltr(
    ages = data[[cols[["ages"]]]],
    cancer = data[[cols[["cancer"]]]],
    cancer_death = data[[cols[["cancer_death"]]]],
    death = data[[cols[["death"]]]],
    pys = data[[cols[["pys"]]]],
    ...
  )
}

calc_ltr_df_by <- function(
  data,
  cols,
  by,
  parallel = FALSE,
  workers = NULL,
  cluster = NULL,
  cache = "retain",
  ...
) {
  data <- as.data.frame(data)
  split_idx <- split(seq_len(nrow(data)), data[by], drop = TRUE)
  dots <- list(...)
  tasks <- Map(
    function(idx, task_id) {
      d <- data[idx, , drop = FALSE]
      group_values <- d[1, by, drop = FALSE]
      list(
        id = task_id,
        ages = d[[cols[["ages"]]]],
        cancer = d[[cols[["cancer"]]]],
        cancer_death = d[[cols[["cancer_death"]]]],
        death = d[[cols[["death"]]]],
        pys = d[[cols[["pys"]]]],
        group_values = group_values,
        group_label = paste(
          paste(by, unlist(group_values, use.names = FALSE), sep = " = "),
          collapse = ", "
        )
      )
    },
    split_idx,
    seq_along(split_idx)
  )
  n_workers <- if (is.null(cluster)) {
    ltr_worker_count(workers, length(tasks), parallel)
  } else {
    length(cluster)
  }

  if (!is.null(cluster) || n_workers > 1L) {
    active_cluster <- cluster
    if (is.null(active_cluster)) {
      active_cluster <- parallel::makePSOCKcluster(n_workers)
      on.exit(parallel::stopCluster(active_cluster), add = TRUE)
    }
    results <- parallel::parLapplyLB(
      active_cluster,
      tasks,
      ltr_group_task,
      dots = dots,
      cache = cache
    )
    results <- results[order(vapply(results, `[[`, integer(1), "id"))]
  } else {
    results <- lapply(
      tasks,
      ltr_group_task,
      dots = dots,
      cache = cache
    )
  }

  failed <- which(!vapply(results, `[[`, logical(1), "ok"))
  if (length(failed) > 0L) {
    first <- results[[failed[[1]]]]
    stop(
      "Error in group ",
      first$group_label,
      ": ",
      first$message,
      call. = FALSE
    )
  }

  out <- do.call(rbind, lapply(results, `[[`, "value"))
  row.names(out) <- NULL
  result_digits <- unique(vapply(
    results,
    `[[`,
    integer(1),
    "digits"
  ))
  if (length(result_digits) == 1L) {
    attr(out, "risk_digits") <- result_digits
  }
  out
}

ltr_group_task <- function(task, dots, cache = "retain") {
  if (cache == "none") {
    on.exit(ltr_forget_caches(), add = TRUE)
  }
  tryCatch(
    {
      ord <- order(task$ages)
      risk <- do.call(
        calc_ltr,
        c(
          list(
            ages = task$ages[ord],
            cancer = task$cancer[ord],
            cancer_death = task$cancer_death[ord],
            death = task$death[ord],
            pys = task$pys[ord]
          ),
          dots
        )
      )
      list(
        ok = TRUE,
        id = task$id,
        digits = as.integer(dots$digits %||% 6L),
        value = cbind(
          task$group_values[rep(1, nrow(risk)), , drop = FALSE],
          risk,
          row.names = NULL
        )
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        id = task$id,
        group_label = task$group_label,
        message = conditionMessage(e)
      )
    }
  )
}

ltr_validate_cluster <- function(cluster) {
  if (is.null(cluster)) {
    return(NULL)
  }
  if (!inherits(cluster, "cluster")) {
    stop("cluster must be NULL or a parallel cluster object")
  }
  cluster
}

ltr_parallel_flag <- function(parallel) {
  if (!is.logical(parallel) || length(parallel) != 1L || is.na(parallel)) {
    stop("parallel must be TRUE or FALSE")
  }
  parallel
}

ltr_worker_count <- function(workers, n_groups, use_parallel) {
  if (!use_parallel || n_groups <= 1L) {
    return(1L)
  }
  if (is.null(workers)) {
    detected <- parallel::detectCores(logical = TRUE)
    if (is.na(detected)) {
      detected <- 2L
    }
    return(as.integer(min(n_groups, max(1L, detected - 1L))))
  }
  if (
    !is.numeric(workers) ||
      length(workers) != 1L ||
      !is.finite(workers) ||
      workers < 1 ||
      workers != as.integer(workers)
  ) {
    stop("workers must be NULL or a positive whole number")
  }
  as.integer(min(workers, n_groups))
}

ltr_group_vars <- function(data, by) {
  if (is.null(by) && inherits(data, "grouped_df")) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("Package 'dplyr' is required for grouped data frames")
    }
    by <- dplyr::group_vars(data)
  }

  if (is.null(by)) {
    return(character())
  }
  if (!is.character(by)) {
    stop("by must be a character vector of grouping column names")
  }
  by
}

ltr_col_name <- function(expr, envir, data_names) {
  if (is.character(expr) && length(expr) == 1) {
    return(expr)
  }
  if (is.name(expr)) {
    expr_name <- as.character(expr)
    if (expr_name %in% data_names) {
      return(expr_name)
    }
  }
  value <- eval(expr, envir = envir)
  if (is.character(value) && length(value) == 1) {
    return(value)
  }
  stop(
    "Data frame column specifications must be column names or character strings"
  )
}
