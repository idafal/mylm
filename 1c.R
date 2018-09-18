
# Select Build, Build and reload to build and lode into the R-session.
library(ggplot2)
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

  # Set class name. This is very important!
  class(est) <- 'mylm'

  # Return the object with all results
  return(est)
}
model1 <- mylm(wages ~ education, data = SLID)

print.mylm <- function(object, ...){
  # Code here is used when print(object) is used on objects of class "mylm"
  # Useful functions include cat, print.default and format
  names <- colnames(object$model)

  cat("Coefficients:\nIntercept: ")
  cat(object$coefficients[1])
  cat("\n")
  cat(names[2])
  cat(": ")
  cat(object$coefficients[2])
}


model1b <- lm(wages ~ education, data = SLID)
print(model1b)
print(model1)

summary.mylm <- function(object, ...){
  # Code here is used when summary(object) is used on objects of class "mylm"
  # Useful functions include cat, print.default and format
  cat('Summary of object\n')
}
model1 <- mylm(wages ~ education, data = SLID)


plot.mylm <- function(object, ...){
  df <- data.frame(object$model)
  names(df) <- c("y","x")
  y_true=df$y
  fitted_values=object$coefficients[1]+df$x*object$coefficients[2]
  residuals=y_true-fitted_values
  data <- data.frame(df,y_true, fitted_values, residuals)
  # Code here is used when plot(object) is used on objects of class "mylm"
  p <- ggplot(data=data, aes(x=fitted_values,y=residuals))+geom_point() #+
        #stat_summary(aes(y = residuals,group=1), fun.y=mean, colour="red", geom="smooth",group=1)
  p

}
plot(model1)

df <- data.frame(model1$model)
(names <- colnames(df))
names(df) <- c("y","x")
df
typeof(names[1])
str(names[1])
name1 <- as.name(names[1])
(df <- rename(df,y=name1))
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
length(colnames(model1$model))

names <- c('en','to','tre')
lapply(names,paste)
?drop
?vector
?I
