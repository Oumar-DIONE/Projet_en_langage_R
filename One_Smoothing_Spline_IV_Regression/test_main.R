# load required libraries
rm(list=ls())
source("DGP_on_the_paper.R")
source("Helper_functions.R")
library(pracma)
library(compiler)
library(orthopolynom)
#library(MASS)
library(foreach)
#library(matrixcalc)
library(parallel)
library(doParallel)
library(doRNG)


rep_fun <- function(n = 4, gnrep = 2, rhouv, sdu, rhoxw, case, fun) {
  res <- foreach(k = 1:2, .combine = 'rbind', .errorhandling = 'remove') %dorng% {
    w <- rnorm(n)
    v <- rnorm(n)
    e <- rnorm(n)
    
    a <- sqrt(rhouv^2 / (1 - rhouv^2))
    u <- -a * v + e
    u <- u / sqrt(1 + a^2)
    u <- sdu * u
    
    betaw <- sqrt(rhoxw^2 / (1 - rhoxw^2))
    x <- betaw * w + v
    x <- x / sqrt(1 + betaw^2)
    
    y <- fun(x, case) + u
    knots <- x
    res_value <- cbind(w = w, v = v, e = e)
    
    return(res_value)
  }
  
  nrow_ <- n * gnrep
  ncol_ <- 3
  data <- res[1:nrow_, 1:3]
  print(data)
  
  data_ <- as.matrix(data)
  return(data_)
}

# Exemple d'utilisation de la fonction
# Vous devez définir les variables rhouv, sdu, rhoxw, case, et fun avant d'appeler my_fun
# Par exemple:
rhouv <- 0.5
sdu <- 1
rhoxw <- 0.5
case <- 1
fun <- function(x, case) { return(x^2 + case) }

datat <- my_fun(n = 4, gnrep = 2, rhouv = rhouv, sdu = sdu, rhoxw = rhoxw, case = case, fun = fun)
print(datat)



# set constants of our problem
enableJIT(1)
N <- c(50) #c(50,60)
Rhouv <- c(.8,.8)
Rhoxw <- c(.7,.7)
kernel <- "laplace"
Cases <- c(rep(2,6), rep(3,6))
Rhouv <- c( c(.5,.5,.8,.8,.8,.8), c(.5,.5,.8,.8,.8,.8))
Rhoxw <- c(c(.9,.9, .9,.9, .7,.7), c(.9,.9, .9,.9, .7,.7))
j<-1

n <- as.numeric(N[j])
sdu <- 1
ll <- 10
pmin <- .00001
pmax <- .7
pp <- seq(pmin,pmax,length=ll)
lambda <- (pp/(1-pp))/(sdu^2)
gnrep <- 2
leval <- 100
nfold <- 2
rhouv <- as.numeric(Rhouv[j]) ## .8 benchmark
rhoxw <- as.numeric(Rhoxw[j]) ## .8 benchmark 
  
case <- as.numeric(Cases[j])

nrcore <- 40
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)

set.seed(1234567)

retrive_res<-function(gnrep=2,n=3)
{
  tic()
  
  res <- foreach(k = 1:gnrep, .combine='rbind', .errorhandling = 'remove') %dorng%{
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
    res_value<-cbind(w=w,v=v,e=e)
    
  }
  #nrow_<-nrow(res)
  #ncol_<-ncol(res)
  data<-res[1,1]
  toc()
  return(data)
}
data_<-retrive_res()
print(data_)
stopCluster(cl)


    
    giv<- rep(0,n)
    sfold <- as.integer(n/nfold)
    W <- wmat(w, h=1 ,ker=kernel,knorm="sq",remove=FALSE)/n
    obj <- rep(0,ll)
    for (j in (1:ll)){
      l <- lambda[j]
      giv<- rep(0,n)
      for (k in (0:(nfold-1))){
        
        Wf <- wmat(w[-((k*sfold+1):((k+1)*sfold))], h=1 ,ker=kernel,knorm="sq",remove=FALSE)/(n-sfold)
        matsf <- tpsmat(x[-((k*sfold+1):((k+1)*sfold))])
        tmatf <- matsf$tmat
        ematf <- matsf$emat
        
        bigmativf <- bmat(l,ematf,tmatf,Wf,n-sfold)
        bigwyf <- c(Wf%*%y[-((k*sfold+1):((k+1)*sfold))],0,0)
        parestivf <- solve(qr(bigmativf,LAPACK=TRUE),bigwyf)
        
        for (i in (((k*sfold+1):((k+1)*sfold)))){
          giv[i] <- parestivf[(n-sfold+1)] + x[i]*parestivf[(n-sfold+2)] + (1/12)*sum(parestivf[1:(n-sfold)]*(abs(x[i]-x[-((k*sfold+1):((k+1)*sfold))]))^3)
        }
        
      }
      civ <- t(y-giv)%*%W%*%(y-giv)
      obj[j] <- civ
      c(M_n=obj[j])
    }
    
    idl <- which(obj==min(obj))
    lcviv <- lambda[idl]
    
    
    # Step 3
    
    l <- lcviv
    
    mats <- tpsmat(x)
    tmat <- mats$tmat
    emat <- mats$emat
    bigmativ <- bmat(l,emat,tmat,W,n)
    bigW <- rbind(W,pracma::zeros(2,n))
    bigwy <- c(W%*%y,0,0)
    parestiv <- solve(qr(bigmativ,LAPACK=TRUE),bigwy)
    giv <-  cbind(emat,t(tmat))%*%parestiv
    
    fitmativ <- pracma::eye(n) - l*solve(qr(W,LAPACK=TRUE),solve(qr(bigmativ,LAPACK=TRUE),bigW)[1:n,1:n])
    df.iv <- as.numeric(sum(diag(fitmativ)))
    
    
  }
  




