test_that("calc_ttest handles NAs and returns expected columns", {
  set.seed(1)
  df <- data.frame(
    y = c(rnorm(50, 0), rnorm(50, 0.5)),
    g = rep(c("A", "B"), each = 50)
  )
  # Inject NAs
  df$y[c(3, 7, 12)] <- NA
  df$g[c(5, 9)] <- NA

  res <- calc_ttest(df, y, g)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("p_value", "ci_lower", "ci_upper") %in% names(res)))
  expect_true(is.numeric(res$p_value))
  expect_true(is.numeric(res$ci_lower))
  expect_true(is.numeric(res$ci_upper))
})

 test_that("calc_ttest errors on wrong group levels", {
  df <- data.frame(y = rnorm(10), g = rep("A", 10))
  expect_error(calc_ttest(df, y, g), "exactly 2 levels")
})

 test_that("calc_ttest errors when var non-numeric", {
  df <- data.frame(y = as.character(1:10), g = rep(c("A","B"), each=5))
  expect_error(calc_ttest(df, y, g), "must be numeric")
})
