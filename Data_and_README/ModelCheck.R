#' Checking Regression Asummptions
#'
#' @param lm a linear model object
#'
#' @return Commandline Statistics and Diagnostic Plots to determine if regression assumptions are met
#' @export
#'
#' @examples lm = lm(y~x,data) ---- ModelCheck(lm) or lm = lm(y~x1+x2,data) ----- ModelChec(lm)

ModelCheck <- function(lm){
  library(s20x)
  print("Plot for checking equality of Variance, mean of 0, and independence of errors")
  eovcheck(lm)
  print("Plot and statistic for checking normality of errors")
  normcheck(lm)
  residuals = residuals(lm)
  shapiro.test(residuals)

  # colnames(X) = c()
  # rownames(X) = c()
  # colnames(Y) = c()
  # rownames(Y) = c()
  # C = solve(t(X)%*%X)
  # beta_hat = C%*%t(X)%*%Y
  # n=dim(X)[1]
  # k = dim(X)[2]-1
  # print(c("Regression Coefficients Starting at beta_0:",beta_hat))
  #
  # SSE = t(Y)%*%Y-t(beta_hat)%*%t(X)%*%Y
  # s2=SSE/(n-(k+1))
  # s = sqrt(s2)
  # print(c("SSE:",SSE,"Residual Standard Error:",s))
  #
  # ttest_vals = numeric(k+1)
  # for(i in 1:(k+1)){
  #   ttest_vals[i]=beta_hat[i]/(s*sqrt(C[i,i]))
  # }
  # ttest_pvals = (1-pt(abs(ttest_vals),(n-(k+1))))*2
  # print(c("T Statistic for each beta term starting at beta0:",ttest_vals))
  # print(c("p value for each T test starting at beta0:",ttest_pvals))
  #
  # SSyy = t(Y)%*%Y-((sum(Y))^2)/n
  # R2 = R2 = 1-SSE/SSyy
  # R2a  = 1-((n-1)/(n-(k+1)))*(1-R2)
  # print(c("R Squared Value:",R2,"Adjusted R squared Value:",R2a))
  #
  # Fstat = (R2/k)/((1-R2)/(n-(k+1)))
  # print(Fstat)
  # F_pval = 1-pf(Fstat,k,(n-(k+1)))
  # print(c("F Statistic Value:",Fstat,"F Test p value:",F_pval))
}
