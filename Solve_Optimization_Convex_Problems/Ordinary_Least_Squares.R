#install.packages("CVXR")
library("CVXR")
library("MASS")
?mvrnorm
## DGP
p<-4
N<-100
mu_<-rep(0,p)
sigma_<-diag(p)
X<-mvrnorm(n=N,mu=mu_,Sigma=sigma_)
eps<-matrix(0.01*rnorm(n=N),nrow=N)
true_beta<-matrix(1:p,nrow =p )
Y<-X%*%true_beta+eps

"" Exemple1: OLS optimization  ""

  ## Optimization
linear_reg_optimization<-function(Y,X)
{
  p<-ncol(X)
  beta<-Variable(rows=p)
  obj<-sum((Y-X%*%beta)^2)
  prob<-Problem(Minimize(obj))
  result<-solve(prob)
  results<-c(min_value=result$value,beta_hat=result$getValue(beta))
  return(results)
}


result<-linear_reg_optimization(Y,X)
    res###optimal values of objective function
result[1]    ### optimal value for the objective function
result[2]    ### optimal value for beta



" Exemple 2: f(x)=x**2+1 "
x<-Variable(1)
obj_2<-x^2+1
prob_2<-Problem(Minimize(obj_2))
result_2<-solve(prob_2)
result_2$value
result_2$getValue(x)


"Exemple3 : OLS problem subject to beta component increasing with th index: that is for 1<=j<=p-1 "
  "beta(j)<=beta(j+1)"
  
beta_3<-Variable(rows=p)
obj_3<-sum((Y-X%*%beta_3)^2)
prob_3<-Problem(Minimize(obj_3),list(diff(beta_3) >=0))
result_3<-solve(prob_3)
     #optimal values of objective function
result_3$value
     #optimal value for beta
result_3$getValue(beta_3)
     #optimal dual values
result_3$getDualValue(constraints(prob_3)[[1]])



"Robust (Huber) Regression "
   ## DGP
## Créer un outlier
outlier_<-10*max(abs(min(Y)),abs(max(Y)))
Y[50]<-outlier_
  ## Resolution du OLS
beta_3_OLS<-Variable(rows=p)
obj_3_OLS<-sum((Y-X%*%beta_3_OLS)^2)
prob_3_ols<-Problem(Minimize(obj_3_OLS))
result_3_OLS<-solve(prob_3_ols)
         # optimal values of objective function
result_3_OLS$value
         # optimal value for beta
result_3_OLS$getValue(beta_3_OLS)
    ## Resolution du OLS robust
M<-outlier_-10


result_3_OLS$getValue(beta_3_OLS)
true_beta
result_3_huber$getValue(beta_3_huber)
huber_reg_optimization<-function(Y,X,M=110)
{
  p<-ncol(X)
  beta_3_huber<-Variable(rows=p)
  obj_3_huber<- sum(CVXR::huber(Y - X%*%beta_3_huber , M))
  prob_3_huber<-Problem(Minimize(obj_3_huber))
  result_3_huber<-solve(prob_3_huber)
  results<-c(min_value_huber=result_3_huber$value,beta_hat_huber=result_3_huber$getValue(beta_3_huber))
  return(results)
}
result_huber<-huber_reg_optimization(Y,X)
###optimal values of objective function
result_huber[1]    ### optimal value for the objective function
result_huber[2]    ### optimal value for beta

