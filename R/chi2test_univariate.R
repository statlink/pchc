chi2test_univariate <- function(x, dc) {
  a <- Rfast::chi2Test_univariate(x, dc)
  a$pvalue <- pchisq(a$statistic, a$df, lower.tail = FALSE)
  a
}
