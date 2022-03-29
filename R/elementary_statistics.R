#' @param xbar sample mean
#' @param s sample standard deviation
#' @param n sample size
#' @param mu0 null hypothesis value
#' @param ha alternative hypothesis. takes on the value of "less than", "greater than", "not equal to"
#' @example 
#' single.mean.test(xbar = 1.91, s = 1.22, n = 305, mu0 = 2.47, ha = "not equal to")
single.mean.test <- function(xbar, s, n, mu0, ha = "not equal to"){
  t.stat <- (xbar - mu0) / (s / sqrt(n))
  cat("The t-statistic is equal to:", t.stat, "\n")
  cat("The SE is equal to:", (s / sqrt(n)), "\n")
  if(ha == "less than") p.value = pt(t.stat, df = n - 1, lower.tail = TRUE)
  if(ha == "greater than") p.value = pt(t.stat, df = n - 1, lower.tail = FALSE)
  if(ha == "not equal to") p.value = pt(abs(t.stat), df = n - 1, lower.tail = FALSE) * 2
  if(p.value < .001) p.value = "< .001"
  cat("The p-value is:", p.value)
}

#' @param xbar sample mean
#' @param s sample standard deviation
#' @param n sample size
#' @param level confidence level
#' @example 
#' single.mean.ci(xbar = 1.91, s = 1.22, n = 305, level = .95)
single.mean.ci <- function(xbar, s, n, level = .95){
  t.ast <- qt(p = 1 - (1 - level)/2, df = n - 1)
  ci <- xbar + c(-1, 1)*t.ast * s / sqrt(n) 
  cat("The ", level * 100, "% confidence interval is equal to: ", round(ci[1], 3), ", ", round(ci[2], 3), "\n", sep = "")
}

#' Summary Statistics by Group
#' @param x the response variable
#' @param group the group variable
#' summary.stats(x = ChickWeight$weight, group = ChickWeight$Diet)
summary.stats <- function(x, group){
  dt <- aggregate(x ~ group, FUN = function(x){c(M = mean(x, na.rm = TRUE),
                                           SD = sd(x,  na.rm = TRUE),
                                           Min = min(x, na.rm = TRUE),
                                           Q1 = quantile(x, probs = .25, na.rm = TRUE),
                                           Median = median(x, na.rm = TRUE),
                                           Q3 = quantile(x, probs = .75, na.rm = TRUE),
                                           Max = max(x, na.rm = TRUE),
                                           n = length(x))})
  dt <- do.call(data.frame, dt)
  colnames(dt) <- c("Group", "Mean", "Std. Dev",  "Min", "Q1", "Median", "Q3", "Max", "n")
  dt
}


#' @param xbar1 sample mean for group 1
#' @param xbar2 sample mean for group 2
#' @param s1 sample standard deviation for group 1
#' @param s2 sample standard deviation for group 2
#' @param n1 sample size for group 1
#' @param n2 sample size for group 2
#' @param mu0 null hypothesis value, defaults to 0
#' @param ha alternative hypothesis. takes on the value of "less than", "greater than", "not equal to"
#' @example 
#' diff.in.mean.test(xbar1 = 328.8912, xbar2 = 293.7657, s1 = 309.8633, s2 = 280.1186, n1 = 239, n2 = 239)
diff.in.mean.test <- function(xbar1, xbar2, s1, s2, n1, n2, mu0 = 0, ha = "not equal to"){
  t.stat <- ((xbar1 - xbar2)- mu0) / sqrt(s1^2 / n1 + s2^2 / n2)
  df <- min(n1, n2) - 1
  cat("The t-statistic is equal to:", t.stat, "\n")
  cat("The SE is equal to:", sqrt(s1^2 / n1 + s2^2 / n2), "\n")
  if(ha == "less than") p.value = pt(t.stat, df = df, lower.tail = TRUE)
  if(ha == "greater than") p.value = pt(t.stat, df = df, lower.tail = FALSE)
  if(ha == "not equal to") p.value = pt(abs(t.stat), df = df, lower.tail = FALSE) * 2
  if(p.value < .001) p.value = "< .001"
  cat("The p-value is:", p.value)
}

#' @param xbar1 sample mean for group 1
#' @param xbar2 sample mean for group 2
#' @param s1 sample standard deviation for group 1
#' @param s2 sample standard deviation for group 2
#' @param n1 sample size for group 1
#' @param n2 sample size for group 2
#' @param level confidence level
#' @example 
#' diff.in.mean.ci(xbar1 = 328.8912, xbar2 = 293.7657, s1 = 309.8633, s2 = 280.1186, n1 = 239, n2 = 239, level = .95)
diff.in.mean.ci <- function(xbar1, xbar2, s1, s2, n1, n2, level = .95){
  df <- min(n1, n2) - 1
  t.ast <- qt(p = 1 - (1 - level)/2, df = df)
  ci <- (xbar1 - xbar2) + c(-1, 1)*t.ast * sqrt(s1^2 / n1 + s2^2 / n2)
  cat("The ", level * 100, "% confidence interval is equal to: ", round(ci[1], 3), ", ", round(ci[2], 3), "\n", sep = "")
}


