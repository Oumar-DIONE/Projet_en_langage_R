compute_E_matrix<-function(sample_data)
{
  n<-length(sample_data)
  n_square<- n*n
  E<-matrix(1:n_square,nrow = n)
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      E[i,j]<-(1/12)*(abs(sample_data[i]-sample_data[j]) **3)
    }
  }
  return(E)
}
compute_E_Tild_matrix<-function(lambda_,E, Omega_matrix)
{
  inv_omega_matrix<-solve(Omega_matrix)
  E_tild<-E+lambda_*inv_omega_matrix
  return(E_tild)
}

M1<-1:4
M1<-matrix(1:4,nrow=2)
M1[1,2]
a_yild<-compute_E_matrix(M1)

compute_P_matrix<-function(Z_data,E_tild)
{ 
  Z_t<-t(Z_data)
  inv_E_Tild<-solve(E_tild)
  P<-Z_data%*%(Z_t%*%(inv_E_Tild%*%Z_dataZ))%*%(Z_t%*%inv_E_Tild)
  return(P)
}