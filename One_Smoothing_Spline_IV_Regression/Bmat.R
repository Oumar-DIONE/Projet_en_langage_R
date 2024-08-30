bmat <- function(l,emat,tmat,W=pracma::eye(n)/n,n)
{
  rbind(cbind(W%*%emat + l*pracma::eye(n),W%*%t(tmat)),cbind(tmat,pracma::zeros(2)))
}

n<-3
lambda<-1
x[1]<-3
x[2]<-2
x[3]<-3
