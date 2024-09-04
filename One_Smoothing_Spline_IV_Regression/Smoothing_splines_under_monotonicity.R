
#  Compute required matrix
#install.packages("pracma")
source("Helper_functions.R")
library(pracma)

retrieve_D_mat<-function(x=seq(1,4,length=4))
{
  n=length(x)
  D_mat<-pracma::zeros(n,n)
  for (i in 1:n)
  {
    for (j in 1:n)
      D_mat[i,j]<-sign(x[i]-x[j])*((x[i]-x[j])^2)

  }
  return(D_mat/4)
}
4*retrieve_D_mat()


#retrieve_D_mat<-function(knots_p=seq(1,4,length=4))  # p pour parameter
#{
  sapply(knots_p, function(x) sign(x-knots_p)*abs(x-knots_p)^2)/4
}
retirev_S_mat<-function(E_mat=pracma::zeros(3,3),omage_mat=pracma::eye(3),tmat=pracma::ones(3,2),l=0.5)
{
E_tild_mat<-E_mat+l*solve(omage_mat)
  S_mat<-rbind(cbind(E_tild_mat,tmat),cbind(t(tmat),pracma::zeros(2,2)))
  return(S_mat)
}
retirev_S_mat()
retrieve_D_0_S_matrix<-function(w=1:4,n=4,p=2,knots_p=seq(-4,-4,length=4),l=0.5)
{
  W_mat<-wmat(w)
  mat <- tpsmat(knots_p)
  tmat <- t(mat$tmat)
  emat <- mat$emat
  #Id_mat<-pracma::eye(n)
  B_mat<-bmat(l,emat,t(tmat),W_mat,n)
  S_mat<-retirev_S_mat(emat,W_mat,tmat,l)
  D_mat<-retrieve_D_mat(knots_p)
  O_mat<-cbind(pracma::zeros(n,1),pracma::ones(n,1))
  D_0_S_mat<-cbind(D_mat,O_mat)%*%S_mat
  print(dim(D_0_S_mat))
  return(D_0_S_mat)
}
retrieve_D_0_S_matrix()


# Installer CVXR si ce n'est pas déjà fait
# install.packages("CVXR")
library(CVXR)

# Définir les variables
p <- 2    # Supposons p=5 pour l'exemple
n <- 4
m<-n # Supposons n=1 pour l'exemple
p_vector <- runif(m)  # Exemple de vecteur p de taille m
Y <- runif(n)  # Exemple de vecteur Y de taille m
C <- retrieve_D_0_S_matrix()  # Matrice C de dimension (m, m + p)
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



seq(1,4,length=4)
