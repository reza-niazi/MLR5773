#' Bayesian Regression
#'
#' @param formula the formula of the model that is desired to be fit
#' @param data the data used for fitting
#'
#' @return regression coefficients as determined by both bayesian and classical regression along with bayesian diagnostic plots/statistics
#' @export
#'
#' @examples BayesRegress(y~x1+x2,data) and BayesRegress(y~x1+x2+x1*x2,data)
BayesRegress <- function(formula,data){
  library(MCMCpack)
  library(ggmcmc)
  library(s20x)
  model_bayes = MCMCregress(formula=formula,burnin=1000,
                      mcmc = 10000,data=data)
  print("Regression coefficients (as posteriors) and quantiles determined by Bayesian Regression")
  summary_bayes = summary(model_bayes)
  print(summary_bayes)
  s=ggs(model_bayes)
  plot(ggs_density(s))
  plot(ggs_traceplot(s))


  model_reg = lm(formula=formula,data=data)
  print("Regression coefficients determined by Classical Regression")
  coeff_reg = model_reg$coefficients
  print(coeff_reg)
  print("Confidence intervals determined by Classial Regression")
  ciReg(model_reg)
}
