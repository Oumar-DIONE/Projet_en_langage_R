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
# Installer CVXR si ce n'est pas déjà fait
# install.packages("CVXR")

toc()

library(CVXR)

# Dimensions
m <- 200  # Dimension de v (et lignes de Y)
p <- 2    # Nombre d'éléments supplémentaires à ajouter à p_Y

# Définir les variables d'optimisation
v <- Variable(m)  # Variable pour l'optimisation

# Exemple de matrice Y de taille (m, n)
 

C <- D_0_S_mat  # Matrice C de dimension (m, m + p)
b <- 1  # b = 1 selon votre contrainte
print(length(v))  # Cela devrait donner 200
print(length(Y)) 
# Produit d'Hadamard entre v et Y
v_Y_hadamard <- v * Y  # Produit élément par élément (Hadamard)
print(length(v_Y_hadamard))

# Compléter v_Y_vector avec des zéros pour obtenir un vecteur de taille (m + p)
v_Y_extended <- c(v_Y_hadamard, rep(0, p))  # Taille (m + p)
print(length(v_Y_extended))
# Convertir v_Y_extended en objet CVXR Constant
v_Y_extended_cvxr <- Constant(v_Y_extended)

# Définir l'objectif (exemple simple)
objective <- Minimize(sum(v))  # Changez cela selon votre objectif

# Définir les contraintes
constraints <- list(v >= 0, sum(v) == 1)  # Ajoutez d'autres contraintes si nécessaire

# Ajouter la contrainte sur p_Y
constraints <- append(constraints, list(C %*% v_Y_extended_cvxr >= pracma::zeros(200)))

# Formuler et résoudre le problème d'optimisation
problem <- Problem(objective, constraints)
result <- tryCatch({
  solve(problem)
}, error = function(e) {
  message("Error occurred: ", e$message)
  NULL
})

# Afficher les résultats
if (!is.null(result)) {
  cat("La solution optimale est:\n")
  print(result$getValue(v))
  cat("La valeur de l'objectif est:\n")
  print(result$value)
} else {
  cat("Erreur lors de la résolution du problème.\n")
}


print(dim(C))
print(length(v_Y_extended_cvxr))
length(Y)





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
print(resultat$getValue(p_var))

print(p+n)
print(dim(A))
print(dim(D_0_S_mat))
