#' Test linear trend in risk across ordered groups
#'
#' `trend_test()` performs a weighted linear trend test for risk estimates
#' across three or more ordered groups. The input should usually be a grouped
#' result returned by [calc_ltr_df()] with `return_variance = TRUE`.
#'
#' @param x A data frame returned by [calc_ltr_df()] with
#'   `return_variance = TRUE`.
#' @param group Character string giving the ordered grouping column in `x`.
#' @param score Optional numeric scores for the ordered groups. If `NULL`,
#'   numeric group values are used directly, factor levels are converted to
#'   `1, 2, ...`, and character groups are scored by order of first appearance
#'   with a warning. A named numeric vector can be used to explicitly map group
#'   values to scores.
#' @param alpha Alpha level for the confidence interval of the slope.
#' @param alternative Alternative hypothesis for the slope. Options are
#'   `"two.sided"`, `"less"`, or `"greater"`.
#' @param digits Integer indicating the number of decimal places used to round
#'   returned estimates and test statistics.
#' @return A data frame with one row per age range containing the number of
#'   groups, trend slope, standard error, Z statistic, P value, and confidence
#'   interval for the slope.
#' @export
trend_test <- function(
  x,
  group,
  score = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  digits = 6
) {
  if (!is.data.frame(x)) {
    stop(
      "x must be a data frame returned by calc_ltr_df(..., return_variance = TRUE)."
    )
  }
  alternative <- match.arg(alternative)
  validate_test_options(alpha, digits)
  group <- trend_group_name(group, names(x))
  trend_validate_input(x, group)

  x$.trend_score <- trend_scores(x[[group]], score = score)

  ranges <- unique(x[, c("start", "end"), drop = FALSE])
  results <- lapply(seq_len(nrow(ranges)), function(i) {
    d <- x[x$start == ranges$start[i] & x$end == ranges$end[i], , drop = FALSE]
    trend_test_one_range(d, group, alpha, alternative)
  })

  out <- do.call(rbind, results)
  row.names(out) <- NULL
  numeric_cols <- vapply(out, is.numeric, logical(1))
  out[numeric_cols] <- lapply(out[numeric_cols], round, digits = digits)
  out
}

trend_test_one_range <- function(d, group, alpha, alternative) {
  if (anyDuplicated(d[[group]]) > 0) {
    stop("Each group must have only one estimate per start/end age range.")
  }
  if (nrow(d) < 3) {
    stop("trend_test requires at least three groups per start/end age range.")
  }
  if (any(d$variance <= 0 | !is.finite(d$variance))) {
    stop("variance must be positive and finite for all groups.")
  }
  if (length(unique(d$.trend_score)) < 2L) {
    stop("score must contain at least two distinct values.")
  }

  design <- cbind(intercept = 1, score = d$.trend_score)
  weights <- 1 / d$variance
  xtwx_inv <- solve(t(design) %*% (design * weights))
  beta <- xtwx_inv %*% t(design) %*% (d$risk * weights)
  slope <- unname(beta["score", 1])
  se <- sqrt(unname(xtwx_inv["score", "score"]))
  z <- slope / se
  p <- trend_p_value(z, alternative)
  z_alpha <- qnorm(1 - alpha / 2)

  data.frame(
    start = d$start[1],
    end = d$end[1],
    n_groups = nrow(d),
    slope = slope,
    se = se,
    z = z,
    p = p,
    lower = slope - z_alpha * se,
    upper = slope + z_alpha * se
  )
}

trend_validate_input <- function(x, group) {
  required <- c(group, "start", "end", "risk", "variance")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop(
      "Missing columns: ",
      paste(missing_cols, collapse = ", "),
      ". Use calc_ltr_df(..., by = ..., return_variance = TRUE)."
    )
  }
}

trend_group_name <- function(group, data_names) {
  if (!is.character(group) || length(group) != 1) {
    stop("group must be a single grouping column name.")
  }
  if (!group %in% data_names) {
    stop("group column not found in x: ", group)
  }
  group
}

trend_scores <- function(group_values, score = NULL) {
  if (is.null(score)) {
    if (is.numeric(group_values)) {
      return(as.numeric(group_values))
    }
    if (is.factor(group_values)) {
      return(as.numeric(group_values))
    }
    warning(
      "Character group values are scored by order of first appearance; consider supplying score explicitly."
    )
    groups <- unique(group_values)
    return(match(group_values, groups))
  }
  if (!is.numeric(score)) {
    stop("score must be numeric.")
  }

  groups <- unique(group_values)
  if (!is.null(names(score))) {
    group_chr <- as.character(group_values)
    missing_scores <- setdiff(unique(group_chr), names(score))
    if (length(missing_scores) > 0) {
      stop(
        "score is missing values for groups: ",
        paste(missing_scores, collapse = ", ")
      )
    }
    return(unname(score[group_chr]))
  }

  if (length(score) != length(groups)) {
    stop("Unnamed score must have one value for each group.")
  }
  score[match(group_values, groups)]
}

trend_p_value <- function(z, alternative) {
  if (alternative == "two.sided") {
    2 * pnorm(-abs(z))
  } else if (alternative == "less") {
    pnorm(z)
  } else {
    pnorm(z, lower.tail = FALSE)
  }
}
