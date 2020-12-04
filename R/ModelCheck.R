#' @title Checking Regression Assumptions
#'
#' @param lm: a linear model object
#'
#' This function checks whether a given linear model satisfies the regression assumptions. That is whether the errors are iid from a N(0,sigma^2*I)
#' distribution. The check on a mean of 0 and constant variance is done by making a residual vs fitted plot. The check on normality is done
#' by making a QQ plot and also running the Shapiro-Wilk Test. The independence of the errors is guaranteed for non time-series data, which is
#' all we will consider. Therefore, the function does not explicitely adress this.
#'
#'
#'
#' @return Commandline Statistics and Diagnostic Plots to determine if regression assumptions are met
#' @export
#'
#' @examples lm = lm(y~x,data) ----> ModelCheck(lm) or lm = lm(y~x1+x2,data) -----> ModelCheck(lm)

ModelCheck <- function(lm){
  library(s20x)
  print("Plot for checking equality of Variance, mean of 0, and independence of errors")
  eovcheck(lm)
  print("Plot and statistic for checking normality of errors")
  normcheck(lm)
  residuals = residuals(lm)
  shapiro.test(residuals)
}
