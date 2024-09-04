
#  Compute required matrix
#install.packages("pracma")
source("Helper_functions.R")
library(pracma)

retrieve_D_mat<-function(x=seq(1,4,length=4))
{
  n=length(x)
  D_mat<-pracma::zeros(n,n)
  for (i in 1:n)
  {
    for (j in 1:n)
      D_mat[i,j]<-sign(x[i]-x[j])*((x[i]-x[j])^2)

  }
  return(D_mat/4)
}
4*retrieve_D_mat()


#retrieve_D_mat<-function(knots_p=seq(1,4,length=4))  # p pour parameter
#{
#  sapply(knots_p, function(x) sign(x-knots_p)*abs(x-knots_p)^2)/4
#}
retriev_S_mat<-function(E_mat=pracma::zeros(3,3),omage_mat=pracma::eye(3),tmat=pracma::ones(3,2),l=0.5)
{
E_tild_mat<-E_mat+l*solve(omage_mat)
  S_mat<-rbind(cbind(E_tild_mat,tmat),cbind(t(tmat),pracma::zeros(2,2)))
  return(S_mat)
}
#retirev_S_mat()
retrieve_D_0_S_matrix<-function(W_mat,mat,knots_p=seq(-4,-4,length=4),l=0.5)
{
  n<-length(knots_p)
  tmat <- mat$tmat
  Z<-t(tmat)
  emat <- mat$emat
  #Id_mat<-pracma::eye(n)
  #B_mat<-bmat(l,emat,t(tmat),W_mat,n)
  S_mat<-retriev_S_mat(emat,W_mat,Z,l)
  D_mat<-retrieve_D_mat(knots_p)
  O_mat<-cbind(pracma::zeros(n,1),pracma::ones(n,1))
  D_0_S_mat<-cbind(D_mat,O_mat)%*%S_mat
  print(dim(cbind(D_mat,O_mat)))
  print(dim(S_mat))
  return(D_0_S_mat)
}
#retrieve_D_0_S_matrix()
knots<-c(1:4)
w_mat<-pracma::eye(4)
mats<-tpsmat(knots)
retrieve_D_0_S_matrix(w_mat,mats,knots)
