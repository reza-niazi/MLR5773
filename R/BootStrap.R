#' @title Bootstrap for Multilinear Regression
#'
#' This function is used to determine regression coefficients and confidence intervals using the bootstrap method. The point and interval estimates are
#' compared to the regular (non-bootstrap) methods. Also, histograms that show the distribution of each regression coefficient through the iterations
#' is shown.
#'
#' @param iter the number of iterations to perform in bootstrap
#' @param alpha the significance level for confidence intervals
#' @param X the design matrix (must have a column of 1's as the first column)
#' @param Y the response vector
#'
#' @return regression coefficients and histogram of regression coefficients vs iterations along with comparison to classical regression approach
#' @export
#'
#' @examples Bootstrap(1000,.05,X,Y) where X and Y are determined by data OR Bootstrap(100000,.1,X,Y)
Bootstrap <- function(iter,alpha,X,Y){
  colnames(X) = c()
  rownames(X) = c()
  ncol = ncol(X)
  nrow=nrow(X)
  beta_matrix = matrix(nrow=iter,ncol=ncol)
  for(k in 1:ncol){
    beta_matrix[,k] = numeric(iter)
  }
  for(i in 1:iter){
    indices = sample(x = 1:nrow, size = nrow, replace = TRUE)
    newX = X[indices,]
    beta_hat = try(solve(t(newX)%*%newX)%*%t(newX)%*%Y,silent=TRUE)

    if(inherits(beta_hat,"try-error")){
      counter = counter+1
    }
    else{
      for(k in 1:ncol){
        beta_matrix[i,k] = beta_hat[k,]
      }
    }
  }
  for(k in 1:ncol){
    hist(x = beta_matrix[,k], probability = TRUE,
         main = paste("Bootstrapped beta",(k-1)),
         xlab = paste("beta",(k-1)," estimates"),col="blue")
  }

  for(k in 1:ncol){
    beta_ave = sum(beta_matrix[,k])/iter
    print(paste("Point Estimate from bootstrap for beta ", (k-1)))
    print(beta_ave)
    print(paste("CI from bootstrap for beta ", (k-1)))
    print(beta_ave*c(1,1)+c(-1,1)*qnorm(1-alpha/2)*sqrt(nrow/(nrow-1))*sqrt(sum((beta_matrix[,k]-beta_ave)**2)/(iter-1)))
  }


  beta_hat = try(solve(t(X)%*%X)%*%t(X)%*%Y,silent=TRUE)

  if(inherits(beta_hat,"try-error")){
    print("A singularity occured when trying to compute the regression coefficients")
  }
  else{
    print("Regression Coefficeints (starting at beta0) from classical ML formulas:")
    print(beta_hat)
    sse = t(Y)%*%Y-t(beta_hat)%*%t(X)%*%Y
    s = sqrt(sse/(nrow-ncol))
    C = solve(t(X)%*%X)
    for(k in 1:ncol)
    {
      print("Confidence interval for regression coefficients from classical ML formulas:")
      print(beta_hat[k]*c(1,1)+c(-1,1)*c(qt(1-alpha/2,(nrow-ncol))*s*sqrt(C[k,k])))
    }
  }
}
