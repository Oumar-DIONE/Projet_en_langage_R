Compute_little_omega_fun<-function(z)
{
  if (z==0)
  {
    w<-1
  }
  else
  {
    w<- (-2*sin(z))/(2*z)
  }
  return(w)
}
compute_omage_matrix<-function(IV_sample)
{
  n<-length(IV_sample)
  n_square<-n*n
  Omage_mat<-matrix(1:n_square,nrow = n)
  for (i in 1:n)
  {
     z_i<-IV_sample[i]
     for (j in 1:n)
     {
       z_j<-IV_sample[i]
       z<-z_i-z_j
       Omage_mat[i,j]<-Compute_little_omega_fun(z)
     }
  }
  return(Omage_mat)
}
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
  n<-nrow(Omega_matrix)
  det_<-det(Omega_matrix)
  if (det_==0)
  {
    print("omega matrix non inversible")
    Omega_matrix<-diag(n)
  }
  
  inv_omega_matrix<-solve(Omega_matrix)
  E_tild<-E+lambda_*inv_omega_matrix
  return(E_tild)
}

p<-1
n<-2
b<-n*p
M1<-1:b
n_square<-n*n
Z<-matrix(1:b,nrow=n)
W<-0.9*Z-0.05
Y<-2*Z+1

compute_P_matrix<-function(Z_data,E_tild)
{ 
  Z_t<-t(Z_data)
  inv_E_Tild<-solve(E_tild)
  P<-Z_data%*%(Z_t%*%(inv_E_Tild%*%Z_data))%*%(Z_t%*%inv_E_Tild)
  return(P)
}
P<-compute_P_matrix(Z,E)

estimated_coefs<-function(Y_data,Z_data,W_data,lambda_)
{
  n<-length(Y_data)
  Omega_matrix_<-compute_omage_matrix(W_data)
  E_<-compute_E_matrix(Z_data)
  E_tild_<-compute_E_Tild_matrix(lambda_,E_,Omega_matrix_)
  P_<-compute_P_matrix(Z_data,E)
  inv_E_tild_<-solve(E_tild_)
  I_matrix<-diag(n)
  g_hat<- (P_+E_%*%inv_E_tild_%*%(I_matrix-P_))%*%Y_data
  return(g_hat)
}

M2<-c(0,1)
estimated_coefs(Y,Z,W,0.05)
