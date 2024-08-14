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
