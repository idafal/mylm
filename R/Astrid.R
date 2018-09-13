
# Select Build, Build and reload to build and lode into the R-session.

mylm <- function(formula, data = list(), contrasts = NULL, ...){
  # Extract model matrix & responses
  mf <- model.frame(formula = formula, data = data)
  X  <- model.matrix(attr(mf, "terms"), data = mf, contrasts.arg = contrasts)
  y  <- model.response(mf)
  terms <- attr(mf, "terms")
  n = nrow(X)
  p = ncol(X)

  # Add code here to calculate coefficients, residuals, fitted values, etc...
  # and store the results in the list est
  beta_hat = solve(t(X)%*%X)%*%t(X)%*%y
  sigma_sq_hat = (1/(n-p))*t(y-X%*%beta_hat)%*%(y-X%*%beta_hat)
  cov_beta_hat = drop(sigma_sq_hat)*solve(t(X)%*%X)
  est <- list(terms = terms, model = mf)
  est$coefficients <- beta_hat
  est$cov <- cov_beta_hat

  # Store call and formula used
  est$call <- match.call()
  est$formula <- formula

  df = n-p
  SSE = t(y-X%*%beta_hat)%*%(y-X%*%beta_hat)
  SST = t(y)%*%(diag(n)-drop(1/n)*rep(1, n)%*%t(rep(1, n)))%*%y
  R2 = 1-SSE/SST
  vec = c(SSE, SST, R2)
  est$vec

  # Set class name. This is very important!
  class(est) <- 'mylm'

  # Return the object with all results
  return(est)
}

print.mylm <- function(object, ...){
  # Code here is used when print(object) is used on objects of class "mylm"
  # Useful functions include cat, print.default and format
  cat("Coefficients:\nIntercept: ")
  cat(object$coefficients[1])
  cat("\nEducation: ")
  cat(object$coefficients[2])
}

summary.mylm <- function(object, ...){
  # Code here is used when summary(object) is used on objects of class "mylm"
  # Useful functions include cat, print.default and format
  cat("Coefficients:\n")
  SD = c(sqrt(object$cov[1,1]), sqrt(object$cov[2,2]))
  Estimates = object$coefficients
  ztest = Estimates/SD
  pvalue = 2*pnorm(-abs(ztest))
  table = data.frame(Estimates, SD, ztest, pvalue)
  row.names(table) = c("Intercept", "Education")
  format(table, digits = 4)
  cat("R squared: ", est$vec[3])
}

plot.mylm <- function(object, ...){
  # Code here is used when plot(object) is used on objects of class "mylm"

}





# This part is optional! You do not have to implement anova
anova.mylm <- function(object, ...){
  # Code here is used when anova(object) is used on objects of class "mylm"

  # Components to test
  comp <- attr(object$terms, "term.labels")

  # Name of response
  response <- deparse(object$terms[[2]])

  # Fit the sequence of models
  txtFormula <- paste(response, "~", sep = "")
  model <- list()
  for(numComp in 1:length(comp)){
    if(numComp == 1){
      txtFormula <- paste(txtFormula, comp[numComp])
    }
    else{
      txtFormula <- paste(txtFormula, comp[numComp], sep = "+")
    }
    formula <- formula(txtFormula)
    model[[numComp]] <- lm(formula = formula, data = object$model)
  }

  # Print Analysis of Variance Table
  cat('Analysis of Variance Table\n')
  cat(c('Response: ', response, '\n'), sep = '')
  cat('          Df  Sum sq X2 value Pr(>X2)\n')
  for(numComp in 1:length(comp)){
    # Add code to print the line for each model tested
  }

  return(model)

}
