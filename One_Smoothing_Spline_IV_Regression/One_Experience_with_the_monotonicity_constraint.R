# dans ce script , je vais générer des données selon le DGP puis faire une oprimisation afin de vérifier que
# mmon script sur le monotonicity marcher pour chaque scénario/expérience lors de la simulations
# Monte Carloe sur le script fine
# Installer CVXR si ce n'est pas déjà fait
# install.packages("CVXR")
rm(list=ls())
source("Helper_functions.R")
source("Smoothing_splines_under_monotonicity.R")
library(pracma)
library(compiler)
#library(MASS)
#library(matrixcalc)

# Step1 : retrive Dta
N <- 200 #c(400,400)
Cases <- 3   #c(2,3)
Rhouv <-  0.8 #seq(0.5,0.8,length=8)#c(.8,.8)
Rhoxw <- 0.8  #seq(0.7,0.9,length=8)  #c(.7,.7)
kernel <- "laplace"
#tic()
set.seed(1234567)
n <- as.numeric(N)
sdu <- 1
ll <- 400 #400
pmin <- .00001
pmax <- .7
pp <- seq(pmin,pmax,length=ll)
lambda <- (pp/(1-pp))/(sdu^2)
l<-lambda[50]
rhouv <- as.numeric(Rhouv) ## .8 benchmark
rhoxw <- as.numeric(Rhoxw) ## .8 benchmark 
case <- as.numeric(Cases)
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

W <- wmat(w, h=1 ,ker=kernel,knorm="sq",remove=FALSE)/n
mats <- tpsmat(x)
tmat <- mats$tmat
emat <- mats$emat

D_0_S_mat<-retrieve_D_0_S_matrix(W,mats,x)
bigmativ <- bmat(l,emat,tmat,W,n)
dim(x)


# Step2 _Retrieve the optimal p_value

library(CVXR)

# Définir la taille
n <- 200  # Par exemple, n colonnes pour Y
p <- 2  # Ajuste cette valeur selon le problème réel

# Créer les variables
p_var <- Variable(n)
Y <-  fun(x,case)  + u  # Y doit être une constante dans ce cas, tu peux le définir comme un vecteur fixe
A <- D_0_S_mat [1:n,1:n]# Remplis cette matrice avec tes données spécifiques
print(length(Y))
print(dim(A))
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
probleme <- Problem(Minimize(objectif), contraintes)

# Résoudre le problème
resultat <- solve(probleme)

# Afficher les résultats
p_start<-resultat$getValue(p_var)
print(p_start)
# ici, on verifie que la somme des poids est bien ègale à 1
print(sum(p_start))

# Step3 :  Compute the optimal solution for our estimator in the case of monotonicity constraint
# Ici nous allons resoudre l'equation normales de notre estimateur classique mais ne remplaçant le Y par p*P
# j'ai ajouté "_start_" à la fin des variables de scripts afin de les différencier avec celles obtnues
# dans le modéle sans hypothèse de monotoncity
g_hat_start<-rep(0,n)
mats_start <- tpsmat(x)
tmat_start <- mats_start$tmat
emat_start <- mats_start$emat
l_start<-0.5
bigmativ_start <- bmat(l_start,emat_start,tmat_start,W,n)
bigW_start <- rbind(W,pracma::zeros(2,n))
p_startY<-p_start*Y
bigwy_start <- c(W%*%p_startY,0,0)  # ici j'ai remplacé Y par P_start*Y
parestiv_start <- solve(qr(bigmativ_start,LAPACK=TRUE),bigwy_start)
g_hat_start <-  cbind(emat_start,t(tmat_start))%*%parestiv_start
print(g_hat_start)
