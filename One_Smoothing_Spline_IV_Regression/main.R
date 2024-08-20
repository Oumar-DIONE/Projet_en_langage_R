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

# set constants of our problem
enableJIT(1)
N <- c(400,400)
Rhouv <- c(.8,.8)
Rhoxw <- c(.7,.7)
kernel <- "laplace"
N <- c(200,400,200,400,200,400, 200,400,200, 400, 200, 400)
Cases <- c(rep(2,6), rep(3,6))
Rhouv <- c( c(.5,.5,.8,.8,.8,.8), c(.5,.5,.8,.8,.8,.8))
Rhoxw <- c(c(.9,.9, .9,.9, .7,.7), c(.9,.9, .9,.9, .7,.7))

estimate_training<-function(N=c(10,12),nrcore=40,seed_=1234567,sdu = 1,ll = 3,pmin =.00001,pmax = .7,gnrep = 2,leval = 100,nfold = 2)
{
  for(j in 1:length(N))
  {
    tic()
    
    cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
    registerDoParallel(cl)
    
    set.seed(seed_)
    n <- as.numeric(N[j])
    pp <- seq(pmin,pmax,length=ll)
    lambda <- (pp/(1-pp))/(sdu^2)
    rhouv <- as.numeric(Rhouv[j]) ## .8 benchmark
    rhoxw <- as.numeric(Rhoxw[j]) ## .8 benchmark 
    case <- as.numeric(Cases[j])
    filename <- paste ("Grid2_tps_galherm_tik_comp", "_rhoxw" , rhoxw,"_rhouv", rhouv , "_case",case,"n",n, ".R" ,sep = "")
    filenameplot <- paste ("Grid2_tps_galherm_tik_comp","_rhoxw" , rhoxw,"_rhouv", rhouv   , "_case",case,"n",n,".pdf",sep = "")
    
    
    res <- foreach(k = 1:gnrep, .combine='cbind', .errorhandling = 'remove') %dorng%
      {
      sample_<-retrieve_data()
      y<-sample_$y
      x<-sample_$x
      w<-sample_$w
      knots <- x
      
      giv<- rep(0,n)
      sfold <- as.integer(n/nfold)
      W <- wmat(w, h=1 ,ker=kernel,knorm="sq",remove=FALSE)/n
      obj <- rep(0,ll)
      for (j in (1:ll))
        {
        l <- lambda[j]
        giv<- rep(0,n)
        for (k in (0:(nfold-1)))
          {
          
          Wf <- wmat(w[-((k*sfold+1):((k+1)*sfold))], h=1 ,ker=kernel,knorm="sq",remove=FALSE)/(n-sfold)
          matsf <- tpsmat(x[-((k*sfold+1):((k+1)*sfold))])
          tmatf <- matsf$tmat
          ematf <- matsf$emat
          
          bigmativf <- bmat(l,ematf,tmatf,Wf,n-sfold)
          bigwyf <- c(Wf%*%y[-((k*sfold+1):((k+1)*sfold))],0,0)
          parestivf <- solve(qr(bigmativf,LAPACK=TRUE),bigwyf)
          
          for (i in (((k*sfold+1):((k+1)*sfold))))
            {
            giv[i] <- parestivf[(n-sfold+1)] + x[i]*parestivf[(n-sfold+2)] + (1/12)*sum(parestivf[1:(n-sfold)]*(abs(x[i]-x[-((k*sfold+1):((k+1)*sfold))]))^3)
           
            warnings()}
          
          }
     toc()
     
         }
    
    print('well done')
    }
    save(res, file = "resultats.RData")
  }
}
estimate_training()