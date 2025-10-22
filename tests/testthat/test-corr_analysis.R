test_that("corr_analysis handles NAs and returns expected columns", {
  set.seed(2)
  x <- rnorm(100)
  y <- 0.5 * x + rnorm(100)
  df <- data.frame(x = x, y = y)
  # Inject NAs
  df$x[c(4, 10)] <- NA
  df$y[c(8, 12)] <- NA

  res <- corr_analysis(df, x, y)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("correlation", "p_value") %in% names(res)))
  expect_true(is.numeric(res$correlation))
  expect_true(is.numeric(res$p_value))
})

 test_that("corr_analysis errors on non-numeric inputs", {
  df <- data.frame(x = letters[1:5], y = 1:5)
  expect_error(corr_analysis(df, x, y), "must be numeric")
})

 test_that("corr_analysis errors on too few complete cases", {
  df <- data.frame(x = c(1, 2), y = c(3, 4))
  expect_error(corr_analysis(df, x, y), "at least 3 complete observations")
})
