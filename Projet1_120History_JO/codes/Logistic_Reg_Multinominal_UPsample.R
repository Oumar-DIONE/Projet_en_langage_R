# Chargement des bibliothèques nécessaires
library(caret)    # Pour l'entraînement des modèles
library(dplyr)    # Pour la manipulation des données
library(nnet)     # Pour le modèle multinomial
library(ggplot2)  # Pour la visualisation

# Supposons que vous avez déjà chargé vos données dans model1_work_df

# Assurez-vous que la variable cible est un facteur avec les bons niveaux
Log_Red_data=model1_work_df[1:500,]
Log_Red_data$category_Medal <- factor(Log_Red_data$category_Medal)

## Sur-échantillonnage
Log_Red_dataUS <- df_trainUS %>%group_by(category_Medal) %>%sample_n(max(table(df_trainUS$category_Medal)), replace = TRUE) %>%ungroup()

print("la distribution des données avant le sur -echantillonnage")
print(table(Log_Red_data$category_Medal))
print("la distribution des données avaprés  le sur -echantillonnage")
print(table(Log_Red_dataUS$category_Medal))

# Division des données en ensembles d'entraînement et de test
set.seed(123)  # Pour la reproductibilité des résultats
LogReg_trainIndexUS <- createDataPartition(Log_Red_dataUS$category_Medal, p = .9, list = FALSE,times=1)
                                  
LogReg_df_trainUS <- Log_Red_dataUS[ LogReg_trainIndexUS,]
LogReg_df_testUS  <- Log_Red_dataUS[-LogReg_trainIndexUS,]

# Entraînement du modèle multinomial
model <- train(category_Medal ~ ., data = LogReg_df_trainUS, method = "multinom", trControl = trainControl(method = "cv", number = 5))

# Prédictions sur les données de test
LogReg_predictions_US <- predict(model, newdata = LogReg_df_testUS)

# Calcul de la matrice de confusion
conf_matrix <- confusionMatrix(LogReg_predictions_US,LogReg_df_testUS$category_Medal)

# Affichage de la matrice de confusion
print("Matrice de confusion :")
print(conf_matrix)
warning()
# Affichage des prédictions et des valeurs réelles
results <- data.frame(Actual = LogReg_df_testUS$category_Medal, Predicted =LogReg_predictions_US)
results$Actual<-as.numeric(as.character(results$Actual) )
results$Predicted<-as.numeric(as.character(results$Predicted))
print("Comparaison des prédictions vs valeurs réelles :")
print(results)
warning()
# Graphique : Comparaison des prédictions et des valeurs réelles
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Valeurs réelles", y = "Prédictions", title = "Comparaison des prédictions vs valeurs réelles")
warning()
