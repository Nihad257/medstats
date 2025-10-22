#' Perform a Two-Sample T-Test
#'
#' This function performs a two-sample t-test to compare means between two groups.
#'
#' Rows with missing values in either `var` or `group_var` are removed prior to
#' analysis. The test requires exactly two non-missing groups and a numeric
#' outcome variable.
#'
#' @param data Data frame containing the two groups
#' @param var Name of the variable to test (numeric)
#' @param group_var Name of the grouping variable (must have 2 levels)
#' @return Tibble with the p-value and 95% confidence interval
#' @export

calc_ttest <- function(data, var, group_var) {
  requireNamespace('dplyr', quietly = TRUE)
  requireNamespace('stats', quietly = TRUE)
  requireNamespace('tibble', quietly = TRUE)

  var_data <- dplyr::pull(data, {{ var }})
  group_data <- dplyr::pull(data, {{ group_var }})

  if (!is.numeric(var_data)) {
    stop('var must be numeric')
  }

  # Remove rows with missing values in either variable
  complete_idx <- stats::complete.cases(var_data, group_data)
  var_clean <- var_data[complete_idx]
  group_clean <- group_data[complete_idx]

  unique_groups <- unique(group_clean)
  if (length(unique_groups) != 2) {
    stop('group_var must have exactly 2 levels after removing NAs')
  }

  # Perform two-sample t-test on cleaned vectors
  group_clean <- as.factor(group_clean)
  ttest_result <- stats::t.test(var_clean ~ group_clean, conf.level = 0.95)

  tibble::tibble(
    p_value = ttest_result$p.value,
    ci_lower = ttest_result$conf.int[1],
    ci_upper = ttest_result$conf.int[2]
  )
}
