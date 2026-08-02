#' Format risk estimates with 95% confidence intervals
#'
#' `format_risk_ci()` combines risk estimates and their lower and upper
#' confidence interval bounds into a character vector suitable for tables. When
#' `risk` is a data frame, the formatted value is added as a new column.
#'
#' @param risk Numeric vector of risk estimates, or a data frame containing risk
#'   estimates and confidence interval bounds.
#' @param lower Numeric vector of lower confidence interval bounds. When `risk`
#'   is a data frame, this can be the lower-bound column name.
#' @param upper Numeric vector of upper confidence interval bounds. When `risk`
#'   is a data frame, this can be the upper-bound column name.
#' @param risk_col Column name containing risk estimates when `risk` is a data
#'   frame. Default is `"risk"`.
#' @param lower_col Column name containing lower confidence interval bounds when
#'   `risk` is a data frame. Default is `"lower"`.
#' @param upper_col Column name containing upper confidence interval bounds when
#'   `risk` is a data frame. Default is `"upper"`.
#' @param name Column name for the formatted output when `risk` is a data frame.
#'   Default is `"risk_95ci"`.
#' @param digits Optional integer indicating the number of decimal places to
#'   display. If `NULL` and `risk` is a data frame returned by [calc_ltr()] or
#'   [calc_ltr_df()], the calculation's `digits` setting is inherited. Otherwise,
#'   values are converted with [as.character()] without additional rounding.
#' @param conf_level Confidence level displayed in the formatted text. Default
#'   is `95`.
#' @param show_conf_level Logical. If `TRUE`, include the confidence level label
#'   such as `95% CI` before the interval bounds. Default is `FALSE`.
#' @param sep Separator between lower and upper confidence interval bounds.
#'   Default is `", "`.
#' @param na Character value used when any of `risk`, `lower`, or `upper` is
#'   missing for an observation. Default is `NA_character_`.
#'
#' @returns A character vector in the form `risk (lower, upper)`, or a
#'   data frame with an added formatted column when `risk` is a data frame.
#' @export
#'
#' @examples
#' format_risk_ci(risk = 12.3, lower = 10.1, upper = 14.5)
#' format_risk_ci(risk = 12.345, lower = 10.123, upper = 14.567, digits = 1)
#'
#' breast <- seer_example_data[seer_example_data$site == "Breast", ]
#' res <- calc_ltr(
#'   ages = breast$ages, cancer = breast$cancer,
#'   cancer_death = breast$cancer_death, death = breast$death,
#'   pys = breast$pys, maj_method = "constant",
#'   ci_method = "delta", age_start = 0, age_end = Inf,
#'   digits = 2
#' )
#' res$risk_95ci <- format_risk_ci(res$risk, res$lower, res$upper, digits = 2)
#' format_risk_ci(res, digits = 2)
format_risk_ci <- function(
  risk,
  lower = NULL,
  upper = NULL,
  risk_col = "risk",
  lower_col = "lower",
  upper_col = "upper",
  name = "risk_95ci",
  digits = NULL,
  conf_level = 95,
  show_conf_level = FALSE,
  sep = ", ",
  na = NA_character_
) {
  if (missing(risk)) {
    stop("risk must be supplied")
  }

  if (is.data.frame(risk)) {
    data <- risk
    if (is.null(digits)) {
      digits <- attr(data, "risk_digits", exact = TRUE)
    }
    cols <- c(risk_col, lower_col, upper_col)
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols) > 0) {
      stop("Missing columns in risk: ", paste(missing_cols, collapse = ", "))
    }

    data[[name]] <- format_risk_ci(
      risk = data[[risk_col]],
      lower = data[[lower_col]],
      upper = data[[upper_col]],
      digits = digits,
      conf_level = conf_level,
      show_conf_level = show_conf_level,
      sep = sep,
      na = na
    )
    return(data)
  }

  if (is.null(lower) || is.null(upper)) {
    stop("lower and upper must be supplied when risk is not a data frame")
  }

  lengths <- c(length(risk), length(lower), length(upper))
  if (!all(lengths == lengths[1])) {
    stop("risk, lower, and upper must have the same length")
  }

  if (!is.null(digits)) {
    if (
      !is.numeric(digits) || length(digits) != 1 || is.na(digits) || digits < 0
    ) {
      stop("digits must be NULL or a non-negative integer")
    }
    digits <- as.integer(digits)
  }

  format_value <- function(x) {
    if (is.null(digits)) {
      return(as.character(x))
    }
    formatC(x, format = "f", digits = digits)
  }

  missing_value <- is.na(risk) | is.na(lower) | is.na(upper)
  ci_label <- if (isTRUE(show_conf_level)) {
    paste0(conf_level, "% CI ")
  } else {
    ""
  }
  out <- paste0(
    format_value(risk),
    " (",
    ci_label,
    format_value(lower),
    sep,
    format_value(upper),
    ")"
  )
  out[missing_value] <- na
  out
}
