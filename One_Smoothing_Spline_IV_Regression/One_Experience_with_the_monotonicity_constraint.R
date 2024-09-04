# dans ce script , je vais générer des données selon le DGP puis faire une oprimisation afin de vérifier que
# mmon script sur le monotonicity marcher pour chaque scénario/expérience lors de la simulations
# Monte Carloe sur le script fine
rm(list=ls())
source("Helper_functions.R")
source("Smoothing_splines_under_monotonicity.R")
library(pracma)
library(compiler)
#library(MASS)
#library(matrixcalc)
N <- 200 #c(400,400)
Cases <- 3   #c(2,3)
Rhouv <-  0.8 #seq(0.5,0.8,length=8)#c(.8,.8)
Rhoxw <- 0.8  #seq(0.7,0.9,length=8)  #c(.7,.7)
kernel <- "laplace"
#tic()
set.seed(1234567)
n <- as.numeric(N)
sdu <- 1
ll <- 20 #400
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
bigW <- rbind(W,pracma::zeros(2,n))
bigwy <- c(W%*%y,0,0)

# Installer CVXR si ce n'est pas déjà fait
# install.packages("CVXR")
library(CVXR)

# Définir les variables
p <- 2    # Supposons p=5 pour l'exemple
m<-n # Supposons n=1 pour l'exemple
p_vector <- runif(m)  # Exemple de vecteur p de taille m
Y <- runif(n)  # Exemple de vecteur Y de taille m
C <- D_0_S_mat  # Matrice C de dimension (m, m + p)
b <- 1  # b = 1 selon votre contrainte

# Variable d'optimisation
v <- Variable(m)

# Définir l'objectif f_0(v)
objective <- Minimize(n - sum(sqrt(n * v)))

# Définir les contraintes
constraints <- list(v >= 0, sum(v) == 1)

# Produit d'Hadamard entre p et Y
p_Y <- p_vector * Y  # vecteur de taille m

# Compléter p_Y par des zéros pour obtenir un vecteur de taille m + p
p_Y_extended <- c(p_Y, rep(0, p))  # vecteur de taille (m + p)

# Convertir p_Y_extended en objet CVXR Constant pour pouvoir faire le produit matriciel
p_Y_extended_cvxr <- Constant(p_Y_extended)

# Ajouter la contrainte supplémentaire d'inégalité : C * (p * Y, 0) <= 0
constraints <- append(constraints, list(C %*% p_Y_extended_cvxr >= 0))

# Formuler le problème d'optimisation
problem <- Problem(objective, constraints)

# Résoudre le problème
result <- solve(problem)

# Afficher la solution
cat("La solution optimale est:\n")
print(result$getValue(v))
cat("La valeur de l'objectif est:\n")
print(result$value)

toc()

