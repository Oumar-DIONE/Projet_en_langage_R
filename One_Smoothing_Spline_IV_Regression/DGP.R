g_fun0<-function(z)
{
  value<-sqrt(2)*z/2
  return(value)
  
}

get_y<-function(z,epsilon_)
{
  Y_<-g_fun0(z)+epsilon_
  return(Y_)
}
rho_epsilon_Z<-0.5
rho_W_Z<-0.8
a<-sqrt((rho_epsilon_Z/(1-rho_epsilon_Z)))
beta<-sqrt((rho_W_Z/(1-rho_W_Z)))
N_sample<-50
get_input_data<-function(a,b,N)
{
  V_sample<-rnorm(N)  
  Eta_sample<-rnorm(N)
  W_sample<-rnorm(N)
  Epsilon_sample<-(a*V_sample+Eta_sample)/sqrt(1+a**2)
  Z_sample<-(b*W_sample+V_sample)/sqrt(1+b**2)
  Y_sample <- sapply(seq_along(Z_sample), function(i) get_y(Z_sample[i], Epsilon_sample[i]))
  Y_sample<-matrix(Y_sample,ncol = 1)
  Z_sample<-matrix(Z_sample,ncol = 1)
  W_sample<-matrix(W_sample,ncol = 1)
  return(list(output=Y_sample,input=Z_sample,Iv=W_sample))

}
my_sample<-get_input_data(a,beta,N_sample)
Y_data<-my_sample$output
Z_data<-my_sample$input
W_data<-my_sample$Iv


