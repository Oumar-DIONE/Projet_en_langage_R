tpsmat <- function(knots)
{
  k <- length(knots)
  tmat <- rbind(pracma::ones(1,k),t(knots))
  emat <- sapply(knots, function(x) abs(x-knots)^3)/12
  
  list(tmat=tmat,emat=emat)
}

knots<-1:4
big_Z_mat<-tpsmat(knots)$tmat
print(big_Z_mat)
big_E_mat<-12*tpsmat(knots)$emat
print(big_E_mat)

