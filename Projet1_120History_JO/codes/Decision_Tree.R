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

# Vérifier la structure des données initiales
print("Structure des données initiales:")
str(model1_work_df)

# Créer un sous-ensemble de données de travail (augmenter la taille si possible)
df_work11 <- model1_work_df[1:500, ]

# Vérifier la distribution des classes dans les données complètes
print(table(df_work11$category_Medal))



# 2. Stratification
#Utilisez la stratification lors de la division des données pour garantir que chaque classe 
#est représentée proportionnellement dans les ensembles d'entraînement et de test :
set.seed(123) # Pour la reproductibilité
trainIndex <- createDataPartition(df_work11$category_Medal, p = .8, 
                                  list = FALSE, 
                                  times = 1, 
                                  groups = 10) # Stratifie la partition en 10 groupes
df_train <- df_work11[ trainIndex,]
df_test  <- df_work11[-trainIndex,]
# Vérifier la distribution des classes dans les ensembles d'entraînement et de test
print("la distribution de Xtrain avant  le sur -echantillonnage")
print(table(df_train$category_Medal))
#print(table(df_test$category_Medal))

#3. Rééchantillonnage
#Si certaines classes sont très peu représentées, envisagez un sur-échantillonnage ou un sous-échantillonnage pour équilibrer les classes :


##Exécution du code avec vérification
## CAS :avec la stratification et la vérification des distributions 

# Assurez-vous que la variable de classe est un facteur avec les mêmes niveaux
df_train$category_Medal <- factor(df_train$category_Medal)
df_test$category_Medal <- factor(df_test$category_Medal, levels = levels(df_train$category_Medal))

# Entraînement du modèle
# Utilisons un modèle de forêt aléatoire pour cet exemple
model <- train(category_Medal ~ ., data = df_train, method = "rf", ntree = 10)

# Évaluation du modèle
predictions <- predict(model, newdata = df_test)
conf_matrix <- confusionMatrix(predictions, df_test$category_Medal)

# Afficher la matrice de confusion
print(conf_matrix)

# Afficher les avertissements
warnings()

