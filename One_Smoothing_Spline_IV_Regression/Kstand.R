# Kernel and kernel matrix
kstand <- function(x,ker='normal',knorm="sq")
{
  # Densities such as the integral of the square of the density is one if knorm is sq 
  # or such that the sd is one if knorm is sd.
  if (ker=='normal') 
  { 
    s <- switch(knorm, sd = 1,sq = 1/(2*sqrt(pi)))
    aux <- exp(- x^2 /(2*s^2)) / (sqrt(2*pi)*s)
  }
  if (ker=='triangle')
  {
    s <- switch(knorm,
                sd = sqrt(6),
                sq = (2/3))
    aux <- (s-abs(x))
    aux <- aux * (aux >0)/s^2
  }
  if (ker=='laplace')
  {
    s <- switch(knorm,
                sd = 1/sqrt(2),
                sq = 1/4)
    aux <- exp(-abs(x)/s)/(2*s)
  }
  aux
}
f_0<-kstand(x=0,ker = 'laplace',knorm = "sq")
print(f_0)



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

2

x<-rnorm(3)
x[1]<-3
x[2]<-2
x[3]<-3
ndims(x)


my_Wmat<-wmat(x)

print(my_Wmat)





sapply(x,function(e) x-e*pracma::ones(3,1))

is.vector(x)


binarizer<-function(cate_sex="homme")
{
  sexe<-switch ( cate_sex,homme=1,femme=0) 
  print(paste("l encodage du sexe est ",sexe))
}

binarizer()
