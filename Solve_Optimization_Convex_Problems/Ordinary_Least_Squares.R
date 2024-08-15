#install.packages("CVXR")
library("CVXR")
library("MASS")
?mvrnorm
p<-2
N<-10
mu_<-rep(0,p)
sigma_<-diag(p)
X<-mvrnorm(n=N,mu=mu_,Sigma=sigma_)
true_beta<-matrix(1:p,nrow =p )
Y<-X%*%true_beta

# Resoluation
beta<-Variable(rows=p)
obj<-sum((Y-X%*%beta)^2)
prob<-Problem(Minimize(obj))
result<-solve(prob)
# optimal values of objective function
result$value
# optimal value for beta
result$getValue(beta)

# Exemple 2: f(x)=x**2
x<-Variable(1)
obj_2<-x^2
prob_2<-Problem(Minimize(obj_2))
result_2<-solve(prob_2)
result_2$value
result_2$getValue(x)

# Exemple3 : OLS problem subject to beta component increasing with th index: that is for 1<=j<=p-1,
# beta(j)<=beta(j+1)
beta_3<-Variable(rows=p)
obj_3<-sum((Y-X%*%beta_3)^2)
prob_3<-Problem(Minimize(obj_3),list(diff(beta_3) >=0))
result_3<-solve(prob_3)
# optimal values of objective function
result_3$value
# optimal value for beta
result_3$getValue(beta_3)
# optimal dual values
result_3$getDualValue(constraints(prob_3)[[1]])



