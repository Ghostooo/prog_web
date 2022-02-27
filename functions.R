remove_outliers <- function(x) {
  x[x %in% boxplot.stats(x)$out] <- mean(x)
  x
}

replace_outliers_k <- function(x, k) {
  quantiles <- quantile(x, na.rm = TRUE)
  # [Q1 - k*(Q3 - Q1), Q3 + k*(Q3 - Q1))]
  EIQ <- quantiles[4] - quantiles[2]
  BI <- quantiles[2] - k*(EIQ)
  BS <- quantiles[4] + k*(EIQ)
  x[x > as.numeric(BS) | x < as.numeric(BI)] <- mean(x)
  x
}