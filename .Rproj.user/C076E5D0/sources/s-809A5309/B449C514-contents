# Part 2 d)
mf <- model.frame(wages ~ education, data = SLID)
X  <- model.matrix(attr(mf, "terms"), data = mf, contrasts.arg = contrasts)
y  <- model.response(mf)
terms <- attr(mf, "terms")
n = nrow(X)
p = ncol(X)
df = n-p
beta_hat = solve(t(X)%*%X)%*%t(X)%*%y
SSE = t(y-X%*%beta_hat)%*%(y-X%*%beta_hat)
SST = t(y)%*%(diag(n)-drop(1/n)*rep(1, n)%*%t(rep(1, n)))%*%y
