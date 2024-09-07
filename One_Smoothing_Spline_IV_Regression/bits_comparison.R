# Kernel and kernel matrix
kstand <- function(x,ker='normal',knorm="sq")
{
  # Densities such as the integral of the square of the density is one if knorm is sq 
  # or such that the sd is one if knorm is sd.
  if (ker=='normal') 
  { 
    s <- switch(knorm,
                sd = 1,
                sq = 1/(2*sqrt(pi)))
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

tpseval <- function(t,knots=knots,a=a,d=d)
{
  cub <- (abs(t-knots)^3)/12
  c(1,t)%*%a + cub%*%d
}

tpseval.prime <- function(t,knots=knots,a=a,d=d){ ## This is the function returning the first derivative of ghat.iv
  ## This is written in the same logic as the tps function above
  sqsign <- (3/12)*((t-knots)^2)*ifelse(t-knots>0,1,-1)
  a[2]+sqsign%*%d
}

bmat <- function(l,emat,tmat,W=pracma::eye(n)/n,n)
{
  rbind(cbind(W%*%emat + l*pracma::eye(n),W%*%t(tmat)),cbind(tmat,pracma::zeros(2)))
}

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

fun.prime <- function(x,case=1){
  switch(case,  
         1,
         x*sqrt(2),
         sqrt(3*sqrt(3))*(1-x^2)*exp(-(x^2)/2),
         sqrt(3)*(sin(pi*x/2)+(x*pi/2)*cos(pi*x/2)),
         4*exp(-abs(x)) , 
         log(abs(x-1)+1)*ifelse(x>1,1,-1)*sqrt(10/3),
         sqrt(2)*( cos(pi*x)-pi*x*sin(pi*x) )
  )
}


wmatp <- function(x, h=1 ,ker='normal',knorm="sq",remove=FALSE)
{
  #   ICM and smoothing matrix 
  #   If no bandwidth is provided for the function, h=1 is used  (no smoothing)
  #   The principle diagonal is replaced with zeros if remove = TRUE.
  n <- dim(x)[1]
  p <- dim(x)[2]
  wmat <- pracma::ones(n,n)
  for (i in 1:p)
  {
    mat <- sapply(x[,i],function(e) x[,i] - e*pracma::ones(n,1))
    wmat <-  wmat*kstand(mat / h,ker=ker,knorm=knorm)/h; # kernel smoothing   
  }
  # principle diagonal of the matrix replaced with zeros
  if (remove==TRUE) wmat <-  wmat - diag(diag(wmat))
  wmat
}

wmat.eval.fun <- function(xdata, xeval , h=1 ,ker='normal', knorm="sd", remove=FALSE)
{
  #   Kernel smoothing matrix having at the row index xeval and column index xdata 
  #   If no bandwidth is provided for the function, h=1 is used  (no smoothing)
  #   The principle diagonal is replaced with zeros if remove = TRUE.
  m <- length(xeval)
  mat <- sapply(xdata,function(e) xeval - e*pracma::ones(m,1))
  wmat <-  kstand(mat / h,ker=ker,knorm=knorm)/h; # kernel smoothing 
  # principle diagonal of the matrix replaced with zeros
  if (remove==TRUE) wmat <-  wmat - diag(diag(wmat))
  wmat
}

RSS <- function(lambda, n, r, A, Astar, AstarA){
  loo.mat <- pracma::ones(n,n) - pracma::eye(n)
  IN <- solve(  as.numeric(lambda)*diag(n) + AstarA  )
  phi.loo <- IN%*%( (Astar*loo.mat)%*%r )
  rss <- (A*loo.mat)%*%phi.loo -  r
  rss <- t(rss)%*%rss
  return(rss)
}

lambda.tikh.fun <- function(y,x,w,hx, hw, lambdamin, lambdamax){
  n <- length(y)
  Kx <- wmat(x=x, h=hx ,ker='normal',knorm="sq",remove=FALSE)
  Kx <- Kx/rowSums(Kx)
  Kw <- wmat(x=w, h=hw ,ker='normal',knorm="sq",remove=FALSE)
  Kw <- Kw/rowSums(Kw)
  r <- Kw%*%y
  KxKw <- Kx%*%Kw
  lambda.star <- pracma::fminbnd(function(e) RSS(e, n=n, r=r, A=Kw, Astar=Kx, AstarA=KxKw ), lambdamin, lambdamax)$xmin    
  ## phihat <- solve(  as.numeric(lambda.star)*diag(n) + KxKw  )%*%Kx%*%r
  Hatmat <- solve(  as.numeric(lambda.star)*diag(n) + KxKw  )%*%Kx%*%Kw
  phihat <- Hatmat%*%y
  df <- sum(diag(Hatmat))
  return(list(phihat=phihat , lambda.star=lambda.star, df=df))
}

phihat.eval.tikh.fun <- function(y,x,w,zeval,hx, hw, lambda, phihat.data){
  Kw <- wmat(x=w, h=hw ,ker='normal',knorm="sq",remove=FALSE)
  Kw <- Kw/rowSums(Kw) 
  Kxeval <- wmat.eval.fun(xdata=x, xeval=zeval , h=hx ,ker='normal', knorm="sd", remove=FALSE)
  Kxeval <- Kxeval/rowSums(Kxeval)
  phihat.eval <- (1/as.numeric(lambda))*(Kxeval%*%Kw%*%(y-phihat.data) )
  return(phihat.eval)
}


hermite.fun <- function(x,d){
  n <- length(x)
  her <- matrix(rep(NA,n*(d+1)), nrow=n,ncol=d+1)
  for(j in 0:d){
    herj <- EQL::hermite(x,j,prob=TRUE)  
    her[,j+1] <- herj
  }
  return(her)
}


ghat.gal.fun <- function(y,x,w, zeval){
  n <- length(y)
  ## tun <- n^(-1/2) #" This is used for tuning , necessary to improve the performance of the galerking estimator
  tun <- 0 #" This is used for tuning , necessary to improve the performance of the galerking estimator
  ## wnorm <- (w-min(w))/(max(w)-min(w)) ## maybe these should be in [-1,1] instead of [0,1]
  ## wnorm <- (2*(w-min(w))/(max(w)-min(w)))-1 ## normalization in [-1,1]
  ## xnorm <- (x-min(x))/(max(x)-min(x)) ## maybe these should be in [-1,1] instead of [0,1]
  ## xnorm <- ( 2*(x-min(x))/(max(x)-min(x)) ) - 1 ## Normalization in [-1,1] 
  nk <- 0
  crit <- -100
  while(crit<0){
    nk <- nk+1
    ## xx <- t(pracma::legendre(nk,xnorm)) ## n by nk matrix
    ## ww <- t(pracma::legendre(nk+1,wnorm)) ## n by (nk+1) matrix
    xx <- hermite.fun(x=x,d=nk)
    ww <- hermite.fun(x=w,d=nk+1)
    ## pmat <- t(xx)%*%( ww%*%solve(qr(t(ww)%*%ww, LAPACK=TRUE))%*%t(ww) )%*%xx
    ## pmat <- t(xx)%*%( ww%*%pracma::pinv(t(ww)%*%ww)%*%t(ww) )%*%xx
    pmat <- t(xx)%*%( ww%*%pracma::pinv(t(ww)%*%ww + tun*diag(nk+2))%*%t(ww) )%*%xx
    #pmat <- ( xx%*%pracma::pinv(t(xx)%*%xx + tun*diag(nk+1))%*%t(xx) )%*%( ww%*%pracma::pinv(t(ww)%*%ww + tun*diag(nk+2))%*%t(ww) ) ## tuned regularizaed version
    ## pmatx <- xx%*%solve(qr(t(xx)%*%xx, LAPACK=TRUE))%*%t(xx)
    ## pmatw <- ww%*%solve(qr(t(ww)%*%ww, LAPACK=TRUE))%*%t(ww)
    ## pmat <- pmatx%*%pmatw
    eigenvalues <- eigen(pmat)$values 
    rho2 <- 1/Re(eigenvalues[which.min(abs(eigenvalues))])
    crit <- rho2*(nk^3.5)/n - 1
  }
  ## qmat <- t(xx)%*%(  ww%*%solve( qr(t(ww)%*%ww, LAPACK=TRUE) )%*%t(ww)  )%*%xx
  ## qmat <- t(xx)%*%(  ww%*%pracma::pinv(t(ww)%*%ww)%*%t(ww)  )%*%xx
  qmat <- t(xx)%*%(  ww%*%pracma::pinv(t(ww)%*%ww+tun*diag(nk+2))%*%t(ww)  )%*%xx ## tuned regularizaed version
  ## nmat <- t(xx)%*%(  ww%*%solve( qr(t(ww)%*%ww, LAPACK=TRUE) )%*%t(ww)  )
  ## nmat <- t(xx)%*%(  ww%*%pracma::pinv(t(ww)%*%ww)%*%t(ww)  )
  nmat <- t(xx)%*%(  ww%*%pracma::pinv(t(ww)%*%ww+tun*diag(nk+2))%*%t(ww)  ) ## tuned regularizaed version
  nmaty <- nmat%*%y
  ## bhat_G <- solve( qr( qmat  , LAPACK=TRUE), nmaty )
  bhat_G <- solve( qr( qmat +tun*diag(nk+1)  , LAPACK=TRUE), nmaty ) ## tuned regularizaed version
  
  phi_hat_G <- xx%*%bhat_G
  Jhatfun <- rep(NA,nk)
  for(j in 1:nk){
    ## xx <- t(pracma::legendre(j, xnorm)) ## n by j matrix
    xx <- hermite.fun(x=x,d=j)
    ## ww <- t(pracma::legendre(j+1,wnorm)) ## n by (j+1) matrix
    ww <- hermite.fun(x=w,d=j+1)
    ## xxinv <- pracma::pinv(t(xx)%*%xx)
    xxinv <- pracma::pinv(t(xx)%*%xx+tun*diag(j+1)) ## tuned regularizaed version
    ## Amin1 <- ww%*%( solve( qr(t(ww)%*%(xx%*%xxinv%*%t(xx))%*%ww, LAPACK=TRUE))%*%t(ww) )
    Amin1 <- ww%*%( solve( qr(t(ww)%*%(xx%*%xxinv%*%t(xx))%*%ww+tun*diag(j+2), LAPACK=TRUE))%*%t(ww) ) ## tuned regularizaed version
    ## qmat <- t(xx)%*%(  ww%*%solve( qr(t(ww)%*%ww, LAPACK=TRUE) )%*%t(ww)  )%*%xx 
    ## qmat <- t(xx)%*%(  ww%*%pracma::pinv(t(ww)%*%ww)%*%t(ww)  )%*%xx
    qmat <- t(xx)%*%(  ww%*%pracma::pinv(t(ww)%*%ww+tun*diag(j+2))%*%t(ww)  )%*%xx ## tuned regularizaed version
    ## nmat <- t(xx)%*%(  ww%*%solve( qr(t(ww)%*%ww, LAPACK=TRUE) )%*%t(ww)  )
    ## nmat <- t(xx)%*%(  ww%*%pracma::pinv(t(ww)%*%ww)%*%t(ww)  )
    nmat <- t(xx)%*%(  ww%*%pracma::pinv(t(ww)%*%ww+tun*diag(j+2))%*%t(ww)  ) ## tuned regularizaed version
    nmaty <- nmat%*%y
    ## Jhatfun[j] <- (2/3)*(log(n)/(n^2))*sum( ((y-phi_hat_G)^2)*rowSums((Amin1%*%xx)^2)  ) - sum( (xx%*%solve(qr(qmat, LAPACK=TRUE), nmaty))^2 )
    Jhatfun[j] <- (2/3)*(log(n)/(n^2))*sum( ((y-phi_hat_G)^2)*rowSums((Amin1%*%xx)^2)  ) - sum( (xx%*%solve(qr(qmat+tun*diag(j+1), LAPACK=TRUE), nmaty))^2 ) ## tuned regularizaed version
  }
  nk <- which.min(Jhatfun)
  ## xx <- t(pracma::legendre(nk,xnorm)) ## n by nk matrix
  xx <- hermite.fun(x=x,d=nk)
  ## ww <- t(pracma::legendre(nk+1,wnorm))  ## n by (nk+1) matrix
  ww <- hermite.fun(x=w,d=nk+1)
  ## qmat <- t(xx)%*%(  ww%*%solve( qr(t(ww)%*%ww, LAPACK=TRUE) )%*%t(ww)  )%*%xx
  ## qmat <- t(xx)%*%(  ww%*%pracma::pinv(t(ww)%*%ww)%*%t(ww)  )%*%xx
  qmat <- t(xx)%*%(  ww%*%pracma::pinv(t(ww)%*%ww+tun*diag(nk+2))%*%t(ww)  )%*%xx ## tuned regularizaed version
  ## nmat <- t(xx)%*%(  ww%*%solve( qr(t(ww)%*%ww, LAPACK=TRUE) )%*%t(ww)  )
  ## nmat <- t(xx)%*%(  ww%*%pracma::pinv(t(ww)%*%ww)%*%t(ww)  )
  nmat <- t(xx)%*%(  ww%*%pracma::pinv(t(ww)%*%ww+tun*diag(nk+2))%*%t(ww)  ) ## tuned regularizaed version
  nmaty <- nmat%*%y
  ## bhat_G <- solve( qr( qmat  , LAPACK=TRUE), nmaty )
  bhat_G <- solve( qr( qmat +tun*diag(nk+1) , LAPACK=TRUE), nmaty ) ## tuned regularizaed version
  zevalnorm <- (zeval-min(zeval))/(max(zeval)-min(zeval)) ## Normalization in [0,1]
  ## zevalnorm <- ( 2*(zeval-min(zeval))/(max(zeval)-min(zeval)) ) -1 ## Normalization in [-1,1]
  ## phi_hat_G_zeval <- t(pracma::legendre(nk,zevalnorm))%*%bhat_G ## phihat at the evaluation points
  phi_hat_G_zeval <- hermite.fun(x=zeval,d=nk)%*%bhat_G ## phihat at the evaluation points
  ## phi_hat_G <- t(pracma::legendre(nk,xnorm))%*%bhat_G ## phihat at the data points
  phi_hat_G <- hermite.fun(x=x,d=nk)%*%bhat_G ## phihat at the data points
  Hatmat <- hermite.fun(x=x,d=nk)%*%solve( qr( qmat +tun*diag(nk+1) , LAPACK=TRUE))%*%nmat
  df <- sum(diag(Hatmat))
  return(list(ghat_gal_eval=phi_hat_G_zeval, ghat_gal =phi_hat_G , nk=nk, df=df))
}

fun.sam <- function(x,case=1)  
{
  switch(case,  
         x,
         sqrt(2)*(x^2),
         0.5*sin(1.5*pi*x))
}

## Functions to implement Horowitz' procedure. 


## AAA: Need to install the package "orthopolynom" needed for legendre.basis.fun below (that is in turn needed for Horowitz estimator)


power.mat.fun <- function(x,degree){ ## here x can be a vector of observations on x
  x <- as.vector(x)
  n <- length(x)
  xmat.pow <- matrix(rep(NA,(degree+1)*n), nrow=degree+1, ncol=n)
  for(s in 1:n){
    xvec.pow <- rep(NA,degree+1)
    for(j in 0:(degree)){
      xj <- as.numeric(x[s])^j
      xvec.pow[j+1] <- xj
    }
    xmat.pow[,s] <- xvec.pow
  }
  return(xmat.pow) 
}

legendre.basis.fun <- function(x,degree){
  
  leg.list <- orthopolynom::legendre.polynomials(degree, normalized=TRUE)
  leg <- unlist(leg.list)
  
  leg.mat <- matrix(rep(0,(degree+1)^2), nrow=(degree+1), ncol=(degree+1))
  m0 <- 1
  for(m in 0:(degree)){ ## Build the matrix of coefficients
    a <- rep(0,(degree+1))
    am <- leg[max(m0,1):(m0+m)]
    a[1:(length(am))] <- am ## *sqrt(m+.5) 
    leg.mat[m+1,] <- a
    m0 <- m0+m+1
  }

  power.mat <- power.mat.fun(x=x,degree=degree) ## (degree+1) times n matrix of basis function:: jeneric element \psi_j(X_s) (j=row, s=col)
  legendre.basis <- t(leg.mat%*%power.mat) ## Gives the matrix \cal{X} or \cal{W} of the notes
  return(legendre.basis)
}
## The one below run provides the Horowitz' originary estimator
ghat.horow.fun <- function(y,z,w,zeval){
  PW <- ecdf(w)
  PZ <- ecdf(z)
  z.norm <- PZ(z) ## normalize on [0,1]
  w.norm <- PW(w)
  zeval.norm <- PZ(zeval)
  
  n <- length(y)
  tun <- 0.001 ## try diferent values here, to see the performance
  nk <- 0
  crit <- -100
  
  while(crit<0){ ## This selects the J_n in Horowitz
    nk <- nk+1
    ## xx <- hermite.fun(x=x,d=nk) ## n X nk matrix
    ## ww <- hermite.fun(x=w,d=nk) ## n X nk matrix
    z.input <- 2*z.norm - 1 ## To shift the Legendre polynomials from [-1,1] to [0,1]
    w.input <- 2*w.norm - 1
    xx <- legendre.basis.fun(x=z.input,degree=nk)*sqrt(2) ## n X (nk+1) matrix; \sqrt{2} is used to guarantee the normalization to 1 of the shifted Legendre Polynomials
    ww <- legendre.basis.fun(x=w.input,degree=nk)*sqrt(2) ## n X (nk+1) matrix
    cmat <- t(xx)%*%ww%*%t(ww)%*%xx/(n^2)
    cmat2 <- cmat%*%t(cmat)
    eigenvalues <- eigen(cmat2)$values 
    rho2 <- 1/Re(eigenvalues[which.min(abs(eigenvalues))])
    crit <- rho2*( (nk+1)^3.5)/n - 1 ## The number of basis functions is nk+1 and not nk, as there is the constant basis function!
  }
  
  mhat.coef <- t(ww)%*%y/n
  qmat <- t(ww)%*%xx/n
  g.tilde.coef <-  solve( qr( qmat +tun*diag(nk+1)  , LAPACK=TRUE), mhat.coef )
  g.tilde.coef <- as.vector(g.tilde.coef)
  g.tilde <- xx%*%g.tilde.coef
  
  Ahat.minus1.star.psi.mat <-   solve( qr( qmat +tun*diag(nk+1)  , LAPACK=TRUE), t(ww) )
  Ahat.minus1.star.psi.mat2 <- Ahat.minus1.star.psi.mat^2
  Jhatfun <- rep(NA,nk)
  for(j in 2:(nk+1)){ ## Start from j=2, as j=1 is just the constant 
    ##   j <- 2
    ## xxj <- xx[,1:j]
    ## wwj <- ww[,1:j]
    g.tilde.coef.j <- g.tilde.coef[1:j]
    ## ghatj <- xxj%*%g.tilde.coef.j
    ## gamma.mat.Astarminus1.j <- gamma.mat.Astarminus1[,1:j]
    Ahat.minus1.star.psi.mat2_j <- Ahat.minus1.star.psi.mat2[1:j,]
    ## dim(Ahat.minus1.star.psi.mat2_j)
    Sumj.Ahat.minus1.star.psi.mat2_j <- colSums(Ahat.minus1.star.psi.mat2_j)
    ## length(Sumj.Ahat.minus1.star.psi.mat2_j)
    Jhatfun_j <- (2/3)*(log(n)/(n^2))*sum(  ((y-g.tilde)^2)*Sumj.Ahat.minus1.star.psi.mat2_j) - sum((g.tilde.coef.j)^2)  
    Jhatfun[j-1] <- as.numeric(Jhatfun_j) ## We put j-1 as an index, as we start from j=2
  }
  nk.star <- which.min(Jhatfun)+1 # truncation parameter selected with Horowitz method
  xx_nk <- xx[, 1:(nk.star)] 
  ## ww_nk <- ww[, 1:nk]
  ## xx <- hermite.fun(x=x,d=nk)
  ## ww <- hermite.fun(x=w,d=nk)
  ghat.coef <- g.tilde.coef[1:nk.star]
  ghat.horow.data <- as.vector(xx_nk%*%ghat.coef)
  df.mat <- xx_nk%*%(solve( qr( qmat +tun*diag(nk+1)  , LAPACK=TRUE), t(ww)/n )[1:nk.star, ])
  df <- sum(diag(df.mat))
  
  
  zeval.input <- 2*zeval.norm - 1
  zz.eval <- legendre.basis.fun(x=zeval.input,degree=(nk.star-1))*sqrt(2) ## length(zeval) X nk.star matrix (Recall that legendre basis contain constant which is the 0 degree )
  ghat.horow.eval <- as.vector(zz.eval%*%ghat.coef)
  
  return(list(ghat.horow.eval=ghat.horow.eval , ghat.horow.data=ghat.horow.data, 
              nk=nk.star, df=df))
}



# Function pour trouver la valeur de p_start dans le problème d'optimisation (4.16) du papier


get_pstart<-function(p=2,Y=1:5,A=pracma::eye(5))
{
  # Définir la taille
  n <- length(Y)  # Par exemple, n colonnes pour Y
  # Créer les variables
  p_var <- CVXR::Variable(n)
  #Y <-  fun(x,case)  + u  # Y doit être une constante dans ce cas, tu peux le définir comme un vecteur fixe
  #A <- D_0_S_mat [1:n,1:n]# Remplis cette matrice avec tes données spécifiques
  # Fonction objectif
  objectif <- n - sum(sqrt(n * p_var))
  
  # Construire le vecteur combiné pour la contrainte matricielle
  # Utiliser une variable pour les zéros supplémentaires
  p_Y <- p_var * Y
  # Définir les contraintes
  contraintes <- list(
    sum(p_var) == 1,                    # La somme des p_i est égale à 1
    p_var >= 0,                         # Chaque p_i est >= 0
    A %*% p_Y >= 0              # La contrainte matricielle
  )
  # Formuler le problème
  probleme <- CVXR::Problem(CVXR::Minimize(objectif), contraintes)
  
  # Résoudre le problème
  resultat <- CVXR::solve(probleme)
  
  # Afficher les résultats
  p_start<-resultat$getValue(p_var)
  #print(p_start)
  return(p_start)
}
# Step2 _Retrieve the optimal p_value


p_start<-get_pstart()
