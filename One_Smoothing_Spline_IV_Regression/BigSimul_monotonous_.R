rm(list=ls())
source("bits_comparison.R")
source("Smoothing_splines_under_monotonicity.R")
library(pracma)
library(compiler)
library(orthopolynom)
#library(MASS)
library(foreach)
#library(matrixcalc)
library(parallel)
library(doParallel)
library(doRNG)
enableJIT(1)
N <- c(400,400)
Cases <- c(2,3)
Rhouv <- c(.8,.8)
Rhoxw <- c(.7,.7)
kernel <- "laplace"

N <- c(200,400,200,400,200,400, 200,400,200, 400, 200, 400)
Cases <- c(rep(2,6), rep(3,6))
Rhouv <- c( c(.5,.5,.8,.8,.8,.8), c(.5,.5,.8,.8,.8,.8))
Rhoxw <- c(c(.9,.9, .9,.9, .7,.7), c(.9,.9, .9,.9, .7,.7))


N <- c(200,400)

cases<-4:5


for(j in 1:length(N)){
  
  
  tic()
  nrcore <- 40
  cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
  registerDoParallel(cl)
  
  set.seed(1234567)
  n <- as.numeric(N[j])
  sdu <- 1
  ll <- 200
  ll <- 5
  pmin <- .00001
  pmax <- .7
  pp <- seq(pmin,pmax,length=ll)
  lambda <- (pp/(1-pp))/(sdu^2)
  nrep <- 2
  leval <- 4
  nfold <- 2
  rhouv <- as.numeric(Rhouv[j]) ## .8 benchmark
  rhoxw <- as.numeric(Rhoxw[j]) ## .8 benchmark 
  
  case <- as.numeric(Cases[j])
  filename <- paste ("Grid2_tps_galherm_tik_comp", "_rhoxw" , rhoxw,"_rhouv", rhouv , "_case",case,"n",n, ".R" ,sep = "")
  filenameplot <- paste ("Grid2_tps_galherm_tik_comp","_rhoxw" , rhoxw,"_rhouv", rhouv   , "_case",case,"n",n,".pdf",sep = "")
  
  
  res <- foreach(k = 1:nrep, .combine='cbind', .errorhandling = 'remove') %dorng%{
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
        # 
        yf<-y[-((k*sfold+1):((k+1)*sfold))]
        xf<-x[-((k*sfold+1):((k+1)*sfold))]
        D_0_S_matf<-retrieve_D_0_S_matrix(Wf,matsf,xf)
        Af<D_0_S_matf [1:(n-sfold),1:(n-sfold)]
        p_startf<-get_pstart(p,yf,Af)
        p_startyf<-p_startf*yf
        bigmativf <- bmat(l,ematf,tmatf,Wf,n-sfold)
        bigwyf <- c(Wf%*%p_startyf,0,0)
        parestivf <- solve(qr(bigmativf,LAPACK=TRUE),bigwyf)
        
        for (i in (((k*sfold+1):((k+1)*sfold)))){
          giv[i] <- parestivf[(n-sfold+1)] + x[i]*parestivf[(n-sfold+2)] + (1/12)*sum(parestivf[1:(n-sfold)]*(abs(x[i]-x[-((k*sfold+1):((k+1)*sfold))]))^3)
        }
        
      }
      civ <- t(y-giv)%*%W%*%(y-giv)
      obj[j] <- civ
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
    
    
    zeval <- seq(-2,2, length=leval)
    
    ghativ <- unlist(sapply(zeval,tpseval,knots=knots,a=parestiv[(n+1):(n+2)],d=parestiv[1:n]))
    ghativ.prime <- unlist(sapply(zeval,tpseval.prime,knots=knots,a=parestiv[(n+1):(n+2)],d=parestiv[1:n]))
    
    hx <- sd(x)*(n^(-1/5))
    hw <- sd(w)*(n^(-1/5))
    lambda.tikh <- lambda.tikh.fun(y=y,x=x,w=w,hx=hx, hw=hw, lambdamin=0.0001, lambdamax=0.999)
    lambda.star <- lambda.tikh$lambda.star
    df.tikh <- as.numeric(lambda.tikh$df)
    phihat.eval.tikh <- phihat.eval.tikh.fun(y=y,x=x,w=w,zeval=zeval,hx=hx, hw=hw, lambda=lambda.star, phihat.data=lambda.tikh$phihat)     
    phihat.eval.tikh <- as.vector(phihat.eval.tikh)
    
    
    ghat.horow <-   ghat.horow.fun(y=y,z=x,w=w,zeval=zeval)
    ghat.horow.data <- ghat.horow$ghat.horow.data
    ghat.horow.eval <-   ghat.horow$ghat.horow.eval
    nk.horow <- ghat.horow$nk
    df.horow <- ghat.horow$df
    
    
    cbind(c(ghativ,lcviv,df.iv,0), c(phihat.eval.tikh, lambda.star,df.tikh, 0),
          c(ghat.horow.eval, nk.horow, df.horow,0) , c(ghativ.prime,lcviv,df.iv,0) )
    
  }
  
  save(res,file=filename)
  nrep2 <- ncol(res)/4
  zeval <- seq(-2,2, length=leval)
  fx <- fun(zeval,case)
  fx.prime <- fun.prime(zeval,case)
  ki <- seq(1,4*nrep2,by=4)
  kt <- seq(2,4*nrep2,by=4)
  kg <- seq(3,4*nrep2,by=4)
  kipr <- seq(4,4*nrep2,by=4)
  ghativ <- res[1:leval,ki]
  ghattik <- res[1:leval,kt]
  ghatgal <- res[1:leval,kg]
  ghativ.prime <- res[1:leval,kipr]
  meiv <- apply(ghativ,1,mean)
  metik <- apply(ghattik,1,mean)
  megal <- apply(ghatgal,1,mean)
  meiv.prime <- apply(ghativ.prime,1,mean)
  biasiv <- mean((meiv - fx)^2)
  biastik <- mean((metik - fx)^2)
  biasgal <- mean((megal - fx)^2)
  biasiv.prime <- mean((meiv.prime - fx.prime)^2)
  mvariv <- mean(apply(ghativ,2,function(a) mean((a-meiv)^2)))
  mvartik <- mean(apply(ghattik,2,function(a) mean((a-metik)^2)))
  mvargal <- mean(apply(ghatgal,2,function(a) mean((a-megal)^2)))
  mvariv.prime <- mean(apply(ghativ.prime,2,function(a) mean((a-meiv.prime)^2)))
  mseiv <- mean(apply(ghativ,2, function(a) mean((a - fx)^2)))
  msetik <- mean(apply(ghattik,2, function(a) mean((a - fx)^2)))
  msegal <- mean(apply(ghatgal,2, function(a) mean((a - fx)^2)))
  mseiv.prime <- mean(apply(ghativ.prime,2, function(a) mean((a - fx.prime)^2)))
  madiv <- mean(apply(ghativ,2, function(a) mean(abs(a - fx))))
  madtik <- mean(apply(ghattik,2, function(a) mean(abs(a - fx))))
  madgal <- mean(apply(ghatgal,2, function(a) mean(abs(a - fx))))
  madiv.prime <- mean(apply(ghativ.prime,2, function(a) mean(abs(a - fx.prime))))
  
  # ci <- apply(ghat,1, quantile,probs=c(0.05,0.95))
  # ciiv <- apply(ghativ,1, quantile,probs=c(0.05,0.95))
  # citik <- apply(ghattik,1, quantile,probs=c(0.05,0.95))
  # cigal <- apply(ghatgal,1, quantile,probs=c(0.05,0.95))
  # ciiv.prime <- apply(ghativ.prime,1, quantile,probs=c(0.05,0.95))
  
  simr <- c(biasiv,biastik, biasgal,biasiv.prime, 
            mvariv,mvartik,mvargal, mvariv.prime , 
            mseiv,msetik,msegal, mseiv.prime, 
            madiv, madtik, madgal, madiv.prime)
  names(simr) <- c("biasiv", "bias tikh" , "bias gal" , "biasiv prime" ,
                   "variv", "var tikh" , "var gal", "variv prime" ,
                   "mseiv", "mse tikh" ,  "mse gal", "mseiv prime" ,
                   "madiv", "mad tikh", "mad gal", "madiv prime" )
  namecase <- paste ( "rhoxw="  , rhoxw, " rhouv=", rhouv, " n=", n, " case=", case, sep = "")
  print(namecase)
  options(digits=10)
  print(simr,digits=3)
  print(nrep2)
  loptiv <-  res[leval+1,ki]
  lopttik <- res[leval+1,kt]
  loptgal <- res[leval+1,kg]
  print("Loptiv")
  print(summary(loptiv))
  print("Lopttik")
  print(summary(lopttik))
  print("Loptgal")
  print(summary(loptgal))
}  
toc()
stopCluster(cl)


