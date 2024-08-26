nrcore <- 40
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)
set.seed(1234567) 

rep_fun_good<-function(n=2,gnrep=3)
{
  tic()
  res_test<- foreach(k = 1:gnrep, .combine='rbind', .errorhandling = 'remove') %dorng%{
    w <- rnorm(n)
    v <- rnorm(n)
    e <- rnorm(n)
    r<-rbind(w=w,v=v,e=e)
  }
  toc()
  
  nlig<-nrow(res_test)
  ncol<-ncol(res_test)
  sample<-res_test[1:nlig,1:ncol]
  return(sample)
}
my_sample<-rep_fun_good()
stopCluster(cl)
print(my_sample)





nrcore <- 40
cl <- makeCluster(mc <- getOption("cl.cores", nrcore))
registerDoParallel(cl)
set.seed(1234567)

get_res <- function(p = 2) { 
  tic()
  
  res <- foreach(k = 1:3, .combine='cbind', .errorhandling = 'remove') %dorng% {
    sample_ <- retrieve_data(n = p)
    y <- sample_$y
    x <- sample_$x
    w <- sample_$w
    if (is.null(x) || is.null(y)) {
      stop("x ou y est nul")
    }
    z=x
  }
  
  toc()
  res1 <- res
  return(res1)
}

   


result <- get_res()
print(result)
stopCluster(cl)


get_res <- function(n = 2) {
  # Nous combinons les résultats avec rbind pour préserver les valeurs de x de chaque itération.
  res <- foreach(k = 1:3, .combine='rbind', .errorhandling = 'remove') %dorng% {
    sample_ <- retrieve_data(n = 2)
    x <- sample_$x  # Assurez-vous que sample_$x n'est pas NULL dans retrieve_data
    return(x)
  }
  
  # 'res' contient les 'x' des trois itérations en lignes
  return(res)
}

# Exécuter la fonction
res_fun<-function(n_iter=2,n_sample=2)
{
  res <- foreach(k = 1:n_iter, .combine='rbind', .errorhandling = 'remove') %dorng% {
    sample_ <- retrieve_data(n = n_sample)
    y_c <- sample_$y  # c =current
    x_c <- sample_$x
    w_c <- sample_$w
    return(list(list(y=y_c,x=x_c,w=w_c)))
    
  }
  return(res)
}
my_res<-res_fun()
iteration1 <- my_res[[1]] # Résultats de la première itération
print(iteration1)
x1<-iteration1$x
print(x1)
iteration2 <- res[[2]] # Résultats de la première itération
print(iteration2)
iteration1$x1
print(valeurs)




library(foreach)
library(doRNG)
library(doParallel)

# Configurer le backend parallèle
cl <- makeCluster(detectCores() - 1) # Utilise tous les coeurs sauf un
registerDoParallel(cl)

# Fonction pour obtenir les résultats
res_fun <- function(n_iter = 2, n_sample = 2) {
  res <- foreach(k = 1:n_iter, .combine = 'list', .errorhandling = 'remove') %dorng% {
    # Debug: Impression de l'itération courante
    print(paste("Iteration:", k))
    
    # Récupération des données
    sample_ <- retrieve_data(n = n_sample)
    
    # Debug: Vérification des données récupérées
    print("Sample data retrieved:")
    print(sample_)
    
    y_c <- sample_$y
    x_c <- sample_$x
    w_c <- sample_$w
    
    # Debug: Vérification des valeurs retournées
    print("Returning values:")
    print(list(y = y_c, x = x_c, w = w_c))
    
    list(y = y_c, x = x_c, w = w_c) # Retourne une liste simple
  }
  
  # Arrêter le backend parallèle
  stopCluster(cl)
  
  # Retourner les résultats
  return(res)
}

# Exemple d'utilisation
results <- res_fun(n_iter = 3, n_sample = 2)
print("Results:")
print(results)

# Accéder aux résultats de la première itération
if (length(results) > 0) {
  iteration1 <- results[[1]]
  print("Iteration 1 data:")
  print(iteration1)
}
---------------------
