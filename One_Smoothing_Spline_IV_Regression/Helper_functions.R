library(pracma)
fun <- function(x,case=1)  
{
  switch(case,  
         x,
         x^2/sqrt(2),
         sqrt(3*sqrt(3))*x*exp(-(x^2)/2),
         sqrt(3)*x*sin(pi*x/2),
         4*exp(-abs(x)) , 
         log(abs(x-1)+1)*ifelse(x>1,1,-1)*sqrt(10/3),
         sqrt(2)*x*cos(pi*x),
         (log(abs(x-1)+1)*ifelse(x>1,1,-1)*sqrt(10/3) - 0.6*x+ (x^3)*2)/8
  )
}

wmat <- function(x, h=1 ,ker='normal',knorm="sq",remove=FALSE)
{
  #   ICM and smoothing matrix 
  #   If no bandwidth is provided for the function, h=1 is used  (no smoothing)
  #   The principle diagonal is replaced with zeros if remove = TRUE.
  n <- length(x)
  mat <- sapply(x,function(e) x - e*pracma::ones(n,1))
  wmat <-  kstand(mat / h,ker=ker,knorm=knorm)/h; # kernel smoothing 
  # principle diagonal of the matrix replaced with zeros
  if (remove==TRUE) wmat <-  wmat - diag(diag(wmat))
  wmat
}

tpsmat <- function(knots)
{
  k <- length(knots)
  tmat <- rbind(pracma::ones(1,k),t(knots))
  emat <- sapply(knots, function(x) abs(x-knots)^3)/12
  
  list(tmat=tmat,emat=emat)
}
bmat <- function(l,emat,tmat,W=pracma::eye(n)/n,n)
{
  rbind(cbind(W%*%emat + l*pracma::eye(n),W%*%t(tmat)),cbind(tmat,pracma::zeros(2)))
}