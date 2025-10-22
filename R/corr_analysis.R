#' Calculate Pearson Correlation Coefficient
#'
#' This function calculates the Pearson correlation between two variables.
#'
#' Rows with missing values in either variable are removed prior to analysis.
#' The Pearson correlation test requires at least 3 complete observations.
#'
#' @param data Data frame containing the variables
#' @param x_var Name of the first variable (numeric)
#' @param y_var Name of the second variable (numeric)
#' @return Tibble with the correlation coefficient and p-value
#' @export

corr_analysis <- function(data, x_var, y_var) {
  requireNamespace('dplyr', quietly = TRUE)
  requireNamespace('stats', quietly = TRUE)
  requireNamespace('tibble', quietly = TRUE)

  x_data <- dplyr::pull(data, {{ x_var }})
  y_data <- dplyr::pull(data, {{ y_var }})

  if (!is.numeric(x_data) || !is.numeric(y_data)) {
    stop('x_var and y_var must be numeric')
  }

  # Remove rows with missing values in either variable
  complete_idx <- stats::complete.cases(x_data, y_data)
  x_clean <- x_data[complete_idx]
  y_clean <- y_data[complete_idx]

  if (length(x_clean) < 3) {
    stop('need at least 3 complete observations')
  }

  corr_result <- stats::cor.test(x_clean, y_clean, method = 'pearson')

  tibble::tibble(
    correlation = as.numeric(corr_result$estimate),
    p_value = corr_result$p.value
  )
}
