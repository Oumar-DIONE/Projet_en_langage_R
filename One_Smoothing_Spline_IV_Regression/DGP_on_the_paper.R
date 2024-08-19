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

retrieve_data<-function(rhouv=0.2,rhoxw=0.8,n=10,sdu=1,case=1)
{
  w <- rnorm(n)
  v <- rnorm(n)
  e <- rnorm(n)
  
  a <- sqrt(rhouv^2/(1-rhouv^2))
  u <- - a*v + e
  u <- u/sqrt(1+a^2)
  u <- sdu*u
  betaw <- sqrt(rhoxw^2/(1-rhoxw^2))
  x <- betaw * w + v
  x <- x/sqrt(1+betaw^2)
  
  y <- fun(x,case)  + u
  knots <- x
  sample_<-list(y=y,x=x,w=w)
  return(sample_)
}


