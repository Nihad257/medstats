#' Calculate Pearson Correlation Coefficient
#'
#' This function calculates the Pearson correlation between two variables.
#'
#' @param data Data frame containing the variables
#' @param x_var Name of the first variable
#' @param y_var Name of the second variable
#' @return Tibble with the correlation coefficient and p-value
#' @export

corr_analysis <- function(data, x_var, y_var) {
  requireNamespace('dplyr', quietly = TRUE)
  requireNamespace('stats', quietly = TRUE)

  x_data <- dplyr::pull(data, {{x_var}})
  y_data <- dplyr::pull(data, {{y_var}})

  corr_result <- stats::cor.test(x_data, y_data, method = 'pearson')

  dplyr::tibble(
    correlation = corr_result$estimate,
    p_value = corr_result$p.value
  )
}
