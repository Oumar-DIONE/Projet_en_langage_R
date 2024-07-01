## Les avertissements indiquent que votre modèle de forêt aléatoire rencontre des problèmes car  
## certaines classes sont vides dans les ensembles de données de formation pour certaines répétitions de la 
## validation croisée. Cela est souvent dû à une répartition déséquilibrée des classes dans 
## votre jeu de données. Voici quelques étapes pour aborder et résoudre ce problème :
# 1. Vérifiez la distribution des classes
# Assurez-vous que chaque classe de category_Medal est représentée dans vos ensembles 
# d'entraînement et de test :
# Charger les bibliothèques nécessaires
library(caret)
library(dplyr)
## US pour UpSampling

# Vérifier la structure des données initiales
print("Structure des données initiales:")
str(model1_work_df)

# Créer un sous-ensemble de données de travail (augmenter la taille si possible)
df_work11US <- model1_work_df[1:500, ]

# Vérifier la distribution des classes dans les données complètes
print(table(df_work11US$category_Medal))



# 2. Stratification
#Utilisez la stratification lors de la division des données pour garantir que chaque classe 
#est représentée proportionnellement dans les ensembles d'entraînement et de test :
set.seed(123) # Pour la reproductibilité
trainIndexUS <- createDataPartition(df_work11US$category_Medal, p = .8, 
                                  list = FALSE, 
                                  times = 1, 
                                  groups = 10) # Stratifie la partition en 10 groupes
df_trainUS <- df_work11US[ trainIndexUS,]
df_testUS  <- df_work11US[-trainIndexUS,]
# Vérifier la distribution des classes dans les ensembles d'entraînement et de test
print("la distribution de Xtrain avant  le sur -echantillonnage")
print(table(df_trainUS$category_Medal))
#print(table(df_test$category_Medal))

#3. Rééchantillonnage
#Si certaines classes sont très peu représentées, envisagez un sur-échantillonnage ou un sous-échantillonnage pour équilibrer les classes :
  
  ## Sur-échantillonnage
raw_df_trainUS<-df_trainUS
df_trainUS <- df_trainUS %>%
 group_by(category_Medal) %>%
  sample_n(max(table(df_trainUS$category_Medal)), replace = TRUE) %>%
  ungroup()
print("la distribution de  Xtrain aprés le sur -echantillonnage")
print(table(df_trainUS$category_Medal))
##Exécution du code avec vérification
## CAS :avec la stratification et la vérification des distributions 

# Assurez-vous que la variable de classe est un facteur avec les mêmes niveaux
df_trainUS$category_Medal <- factor(df_trainUS$category_Medal)
df_testUS$category_Medal <- factor(df_testUS$category_Medal, levels = levels(df_trainUS$category_Medal))

# Entraînement du modèle
# Utilisons un modèle de forêt aléatoire pour cet exemple
model <- train(category_Medal ~ ., data = df_trainUS, method = "rf", ntree = 10)

# Évaluation du modèle
predictionsUS <- predict(model, newdata = df_testUS)
conf_matrixUS <- confusionMatrix(predictionsUS, df_testUS$category_Medal)

# Afficher la matrice de confusion
print(conf_matrixUS)

# Afficher les avertissements
warnings()

