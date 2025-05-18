#' Perform a Two-Sample T-Test
#'
#' This function performs a two-sample t-test to compare means between two groups.
#'
#' @param data Data frame containing the two groups
#' @param var Name of the variable to test
#' @param group_var Name of the grouping variable (must have 2 levels)
#' @return Tibble with the p-value and 95% confidence interval
#' @export

calc_ttest <- function(data, var, group_var) {
  requireNamespace('dplyr', quietly = TRUE)
  requireNamespace('stats', quietly = TRUE)

  var_data <- dplyr::pull(data, {{var}})
  group_data <- dplyr::pull(data, {{group_var}})

  if (length(unique(group_data)) != 2) {
    stop('group_var must have exactly 2 levels')
  }

  ttest_result <- stats::t.test(var_data ~ group_data, data = data, conf.level = 0.95)

  dplyr::tibble(
    p_value = ttest_result$p.value,
    ci_lower = ttest_result$conf.int[1],
    ci_upper = ttest_result$conf.int[2]
  )
}
