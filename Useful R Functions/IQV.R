IQV <- function(tbl, k) {
  perc <- tbl * 100
  sumsquared <- sum(perc^2)
  sumsquared <- 10000-sumsquared
  heterogeneity <- (k * sumsquared)/((k-1)*(10000))
  if (is.nan(heterogeneity) == TRUE) heterogeneity = NA
  return(heterogeneity)
}
