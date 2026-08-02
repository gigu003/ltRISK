#' Test the difference in risk between two groups
#'
#' `ztest()` compares risk estimates from two independent groups using a
#' large-sample Z test. The inputs should be results returned by
#' [calc_ltr()] or [calc_ltr_df()] with `return_variance = TRUE`.
#'
#' @param x A data frame returned by [calc_ltr()] with
#'   `return_variance = TRUE`, or a grouped result returned by [calc_ltr_df()].
#' @param y Optional. A second data frame returned by [calc_ltr()] with
#'   `return_variance = TRUE`. If `NULL`, `group` must be supplied and the two
#'   groups are selected from `x`.
#' @param group Optional character string giving the grouping column in `x`.
#'   Used when `y = NULL`.
#' @param ref Reference group value when `group` is supplied.
#' @param compare Comparison group value when `group` is supplied. The reported
#'   difference is `risk_compare - risk_ref`.
#' @param alpha Alpha level for the confidence interval of the difference.
#' @param alternative Alternative hypothesis. Options are `"two.sided"`,
#'   `"less"`, or `"greater"`. For grouped input, the alternative is applied
#'   to `risk_compare - risk_ref`; for two-input usage it is applied to
#'   `risk_x - risk_y`.
#' @param digits Integer indicating the number of decimal places used to round
#'   the returned estimates and test statistics.
#' @return A data frame containing group labels, matched age ranges, risks,
#'   risk difference, standard error of the difference, Z statistic, P value,
#'   and confidence interval for the difference.
#' @export
#'
#' @examples
#' data("seer_example_data")
#' breast <- seer_example_data[seer_example_data$site == "Breast", ]
#' female <- breast[breast$sex == 2, ]
#' male <- breast[breast$sex == 1, ]
#' if (nrow(female) > 0 && nrow(male) > 0) {
#'   risk_female <- calc_ltr(
#'     female$ages, female$cancer, female$cancer_death, female$death,
#'     female$pys, maj_method = "constant", return_variance = TRUE
#'   )
#'   risk_male <- calc_ltr(
#'     male$ages, male$cancer, male$cancer_death, male$death,
#'     male$pys, maj_method = "constant", return_variance = TRUE
#'   )
#'   ztest(risk_male, risk_female)
#' }
ztest <- function(
  x,
  y = NULL,
  group = NULL,
  ref = NULL,
  compare = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  digits = 6
) {
  UseMethod("ztest", x)
}

#' @rdname ztest
#' @method ztest data.frame
#' @export
ztest.data.frame <- function(
  x,
  y = NULL,
  group = NULL,
  ref = NULL,
  compare = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  digits = 6
) {
  alternative <- match.arg(alternative)

  if (is.null(y)) {
    if (is.null(group)) {
      stop("When y is NULL, group must be supplied.")
    }
    group <- ztest_group_name(group, names(x))
    ztest_validate_calc_ltr_result(x, extra_cols = group)

    groups <- unique(x[[group]])
    if (is.null(ref) || is.null(compare)) {
      if (length(groups) != 2) {
        stop(
          "ref and compare must be supplied when group has more than two values."
        )
      }
      ref <- groups[1]
      compare <- groups[2]
    }

    ref <- ztest_match_group_value(ref, groups, "ref")
    compare <- ztest_match_group_value(compare, groups, "compare")
    if (identical(ref, compare)) {
      stop("ref and compare must identify different groups.")
    }

    x_ref <- x[x[[group]] == ref, , drop = FALSE]
    x_compare <- x[x[[group]] == compare, , drop = FALSE]
    return(ztest_compare_frames(
      x = x_compare,
      y = x_ref,
      alpha = alpha,
      alternative = alternative,
      digits = digits,
      group_x = compare,
      group_y = ref,
      risk_x_name = "risk_compare",
      risk_y_name = "risk_ref",
      diff_name = "difference",
      diff_label = "risk_compare - risk_ref"
    ))
  }

  if (!is.data.frame(y)) {
    stop(
      "y must be a data frame returned by calc_ltr(..., return_variance = TRUE)."
    )
  }
  if (!is.null(group)) {
    warning("group is ignored when y is supplied.")
  }
  ztest_validate_calc_ltr_result(x)
  ztest_validate_calc_ltr_result(y)
  ztest_compare_frames(
    x = x,
    y = y,
    alpha = alpha,
    alternative = alternative,
    digits = digits,
    group_x = "x",
    group_y = "y",
    risk_x_name = "risk_x",
    risk_y_name = "risk_y",
    diff_name = "difference",
    diff_label = "risk_x - risk_y"
  )
}

#' Pairwise Z tests for risk differences among groups
#'
#' `pairwise_ztest()` performs pairwise large-sample Z tests for risk estimates
#' across two or more independent groups. The input should be a grouped result
#' returned by [calc_ltr_df()] with `return_variance = TRUE`.
#'
#' @param x A grouped data frame returned by [calc_ltr_df()] with
#'   `return_variance = TRUE`.
#' @param group Character string giving the grouping column in `x`.
#' @param ref Optional reference group value. If `NULL`, all pairwise
#'   comparisons are performed. If supplied, each non-reference group is
#'   compared against `ref`.
#' @param alpha Alpha level for the confidence interval of each difference.
#' @param alternative Alternative hypothesis. Options are `"two.sided"`,
#'   `"less"`, or `"greater"`. The alternative is applied to
#'   `risk_compare - risk_ref`.
#' @param p_adjust_method Method passed to [stats::p.adjust()] for multiplicity
#'   adjustment. Use `"none"` to return unadjusted P values.
#' @param digits Integer indicating the number of decimal places used to round
#'   the returned estimates and test statistics.
#' @return A data frame containing group labels, matched age ranges, risks,
#'   risk difference, standard error of the difference, Z statistic, raw P value,
#'   adjusted P value, and confidence interval for the difference.
#' @export
#'
#' @examples
#' data("seer_example_data")
#' all_sites <- seer_example_data[
#'   seer_example_data$site == "All" & seer_example_data$sex %in% c(1, 2),
#' ]
#' risks <- calc_ltr_df(
#'   all_sites,
#'   by = "sex",
#'   ages = ages,
#'   cancer = cancer,
#'   cancer_death = cancer_death,
#'   death = death,
#'   pys = pys,
#'   maj_method = "constant",
#'   return_variance = TRUE
#' )
#' pairwise_ztest(risks, group = "sex")
pairwise_ztest <- function(
  x,
  group,
  ref = NULL,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  p_adjust_method = "holm",
  digits = 6
) {
  alternative <- match.arg(alternative)
  if (!is.data.frame(x)) {
    stop(
      "x must be a grouped data frame returned by calc_ltr_df(..., return_variance = TRUE)."
    )
  }
  group <- ztest_group_name(group, names(x))
  ztest_validate_calc_ltr_result(x, extra_cols = group)

  groups <- unique(x[[group]])
  if (length(groups) < 2) {
    stop("group must contain at least two values.")
  }

  comparisons <- ztest_pairwise_comparisons(groups, ref)
  results <- lapply(seq_len(nrow(comparisons)), function(i) {
    group_ref <- comparisons$group_ref[[i]]
    group_compare <- comparisons$group_compare[[i]]
    x_ref <- x[x[[group]] == group_ref, , drop = FALSE]
    x_compare <- x[x[[group]] == group_compare, , drop = FALSE]
    ztest_compare_frames(
      x = x_compare,
      y = x_ref,
      alpha = alpha,
      alternative = alternative,
      digits = digits,
      group_x = group_compare,
      group_y = group_ref,
      risk_x_name = "risk_compare",
      risk_y_name = "risk_ref",
      diff_name = "difference",
      diff_label = "risk_compare - risk_ref"
    )
  })

  out <- do.call(rbind, results)
  out$p_adjusted <- stats::p.adjust(out$p, method = p_adjust_method)
  out <- out[, c(
    "group_compare",
    "group_ref",
    "start",
    "end",
    "risk_compare",
    "risk_ref",
    "difference",
    "se",
    "z",
    "p",
    "p_adjusted",
    "lower",
    "upper"
  )]
  row.names(out) <- NULL
  attr(out, "difference") <- "risk_compare - risk_ref"
  attr(out, "p.adjust.method") <- p_adjust_method
  out
}

ztest_pairwise_comparisons <- function(groups, ref = NULL) {
  if (is.null(ref)) {
    pairs <- utils::combn(seq_along(groups), 2)
    return(data.frame(
      group_compare = groups[pairs[2, ]],
      group_ref = groups[pairs[1, ]],
      row.names = NULL
    ))
  }

  ref <- ztest_match_group_value(ref, groups, "ref")
  compare_groups <- groups[groups != ref]
  data.frame(
    group_compare = compare_groups,
    group_ref = rep(ref, length(compare_groups)),
    row.names = NULL
  )
}

ztest_compare_frames <- function(
  x,
  y,
  alpha,
  alternative,
  digits,
  group_x,
  group_y,
  risk_x_name,
  risk_y_name,
  diff_name,
  diff_label
) {
  validate_test_options(alpha, digits)
  if (
    anyDuplicated(x[c("start", "end")]) ||
      anyDuplicated(y[c("start", "end")])
  ) {
    stop("Each input must contain one estimate per start/end age range.")
  }
  matched <- merge(
    x[, c("start", "end", "risk", "variance"), drop = FALSE],
    y[, c("start", "end", "risk", "variance"), drop = FALSE],
    by = c("start", "end"),
    suffixes = c("_x", "_y"),
    sort = FALSE
  )

  if (nrow(matched) != nrow(x) || nrow(matched) != nrow(y)) {
    stop("x and y must contain the same start/end age ranges.")
  }

  difference <- matched$risk_x - matched$risk_y
  se <- sqrt(matched$variance_x + matched$variance_y)
  if (any(!is.finite(se) | se <= 0)) {
    stop("Combined standard errors must be positive and finite.")
  }
  z <- difference / se
  p <- ztest_p_value(z, alternative)
  z_alpha <- qnorm(1 - alpha / 2)
  lower <- difference - z_alpha * se
  upper <- difference + z_alpha * se

  out <- data.frame(
    group_x = group_x,
    group_y = group_y,
    start = matched$start,
    end = matched$end,
    risk_x = matched$risk_x,
    risk_y = matched$risk_y,
    difference = difference,
    se = se,
    z = z,
    p = p,
    lower = lower,
    upper = upper,
    row.names = NULL
  )
  names(out)[names(out) == "group_x"] <- if (risk_x_name == "risk_compare") {
    "group_compare"
  } else {
    "group_x"
  }
  names(out)[names(out) == "group_y"] <- if (risk_y_name == "risk_ref") {
    "group_ref"
  } else {
    "group_y"
  }
  names(out)[names(out) == "risk_x"] <- risk_x_name
  names(out)[names(out) == "risk_y"] <- risk_y_name
  names(out)[names(out) == "difference"] <- diff_name
  attr(out, "difference") <- diff_label

  numeric_cols <- vapply(out, is.numeric, logical(1))
  numeric_cols["p"] <- FALSE
  out[numeric_cols] <- lapply(out[numeric_cols], round, digits = digits)
  out
}

ztest_validate_calc_ltr_result <- function(x, extra_cols = character()) {
  required <- c("start", "end", "risk", "variance", extra_cols)
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop(
      "Missing columns: ",
      paste(missing_cols, collapse = ", "),
      ". Use calc_ltr(..., return_variance = TRUE) or calc_ltr_df(..., return_variance = TRUE)."
    )
  }
  if (
    any(!is.finite(x$risk)) ||
      any(!is.finite(x$variance)) ||
      any(x$variance < 0)
  ) {
    stop("risk and variance must be finite, and variance must be non-negative.")
  }
}

validate_test_options <- function(alpha, digits) {
  if (
    !is.numeric(alpha) ||
      length(alpha) != 1L ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1
  ) {
    stop("alpha must be a number between 0 and 1.")
  }
  if (
    !is.numeric(digits) ||
      length(digits) != 1L ||
      !is.finite(digits) ||
      digits < 0 ||
      digits != as.integer(digits)
  ) {
    stop("digits must be a non-negative whole number.")
  }
  invisible(TRUE)
}

ztest_group_name <- function(group, data_names) {
  if (!is.character(group) || length(group) != 1) {
    stop("group must be a single grouping column name.")
  }
  if (!group %in% data_names) {
    stop("group column not found in x: ", group)
  }
  group
}

ztest_match_group_value <- function(value, groups, arg_name) {
  if (length(value) != 1) {
    stop(arg_name, " must identify exactly one group.")
  }
  if (is.factor(groups)) {
    value <- as.character(value)
    groups_chr <- as.character(groups)
    if (!value %in% groups_chr) {
      stop(arg_name, " not found in group values.")
    }
    return(groups[match(value, groups_chr)])
  }
  if (!value %in% groups) {
    stop(arg_name, " not found in group values.")
  }
  value
}

ztest_p_value <- function(z, alternative) {
  if (alternative == "two.sided") {
    2 * pnorm(-abs(z))
  } else if (alternative == "less") {
    pnorm(z)
  } else {
    pnorm(z, lower.tail = FALSE)
  }
}
