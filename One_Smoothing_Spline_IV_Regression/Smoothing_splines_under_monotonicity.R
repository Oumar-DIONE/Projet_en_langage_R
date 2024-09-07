
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
retriev_S_mat<-function(E_mat=pracma::zeros(3,3),omage_mat=pracma::eye(3),tmat=pracma::ones(3,2),l=0.5,p=2)
{
E_tild_mat<-E_mat+l*solve(omage_mat)
  S_mat<-rbind(cbind(E_tild_mat,tmat),cbind(t(tmat),pracma::zeros(p,p)))
  return(S_mat)
}
#retirev_S_mat()
# experience sur l'ensemble complet des données
retrieve_D_0_S_matrix<-function(W_mat,mat,knots_p=seq(-4,-4,length=4),l=0.5,p=2)
{
  n<-length(knots_p)
  tmat <- mat$tmat
  Z<-t(tmat)
  emat <- mat$emat
  #Id_mat<-pracma::eye(n)
  #B_mat<-bmat(l,emat,t(tmat),W_mat,n)
  S_mat<-retriev_S_mat(emat,W_mat,Z,l,p=p)
  D_mat<-retrieve_D_mat(knots_p)
  O_mat<-pracma::zeros(n,p)
  D_O_mat<-cbind(D_mat,O_mat)
  D_0_S_mat<-D_O_mat%*%S_mat
  print(paste("dim de D_O_mat ",dim(D_O_mat)))
  print(paste("dim de S_mat ",dim(S_mat)))
  return(D_0_S_mat)
}
#retrieve_D_0_S_matrix()
#x <- betaw * w + v
#x <- x/sqrt(1+betaw^2)
#l<-0.5
#W <- wmat(w, h=1 ,ker=kernel,knorm="sq",remove=FALSE)/n
#mats <- tpsmat(x)
#tmat <- mats$tmat
#emat <- mats$emat

#D_0_S_mat<-retrieve_D_0_S_matrix(W,mats,x)
#bigmativ <- bmat(l,emat,tmat,W,n)
#dim(x)
#knots<-1:4
#w_mat<-W
#mats<-tpsmat(x)
#y<-fun(x,case=3)
#D_0_S_matf<-retrieve_D_0_S_matrix(w_mat,mats,x)
#print(dim(D_0_S_matf))
#n<-length(y)
#p<-2
#A<-D_0_S_matf [1:n,1:n]
#p_start<-get_pstart(p,y,A)
#print(sum(p_start))

#p_starty<-p_start*y
#l<-0.5
#bigmativ <- bmat(l,emat,tmat,w_mat,n)
#bigwyf <- c(w_mat%*%p_starty,0,0)
#parestivf <- solve(qr(bigmativ,LAPACK=TRUE),bigwyf)
#print(parestivf)

# experience sur un seul fold
#x <- betaw * w + v
#print(length(x))
#x <- x/sqrt(1+betaw^2)
#xf<-x[1:100]
#l<-0.5
#sfold<-100
#W <- wmat(w, h=1 ,ker=kernel,knorm="sq",remove=FALSE)/n
#Wf<-W[1:sfold,1:sfold]
#matsf <- tpsmat(xf)
#tmatf <- matsf$tmat
#ematf <- matsf$emat
#print(dim(ematf)) 
#print(dim(tmatf))
#D_0_S_matf<-retrieve_D_0_S_matrix(Wf,matsf,xf)
#print(sfold)
#bigmativf <- bmat(l,ematf,tmatf,Wf,sfold)
#dim(bigmativf)
#w_mat<-Wf
#mats<-tpsmat(xf)
#y<-fun(xf,case=3)
#yf<-y[1:sfold]
#print(length(y))
#D_0_S_matf<-retrieve_D_0_S_matrix(w_mat,mats,xf)
#print(dim(D_0_S_matf))
#p<-2
#Af<-D_0_S_matf [1:sfold,1:sfold]
#print(dim(A))
#p_startf<-get_pstart(p,yf,Af)
#print(sum(p_startf))
#p_startyf<-p_startf*yf
#l<-0.5

#bigmativf <- bmat(l,ematf,tmatf,Wf,sfold)
#bigwyf <- c(Wf%*%p_startyf,0,0)
#parestivf <- solve(qr(bigmativf,LAPACK=TRUE),bigwyf)
#print(parestivf)
