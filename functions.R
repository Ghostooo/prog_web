remove_outliers <- function(x) {
  x[x %in% boxplot.stats(x)$out] <- median(x)
}
