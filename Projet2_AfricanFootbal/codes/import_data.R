# setwd("/home/onyxia/work/Projet_en_langage_R/Projet2_AfricanFootbal/codes")
African_Nations_df<-read.csv("/home/onyxia/work/Projet_en_langage_R/Projet2_AfricanFootbal/data/African_Nations_results.csv")
#Tri selon la colonne African_Nations_df
African_Nations_df <- African_Nations_df[order(African_Nations_df$tournament), ]
# Résumé des données
str(African_Nations_df)
# Ajoutez la colonne qui définis la différence de but par rapport à l'"quipe domicile
African_Nations_df$GA<-African_Nations_df$home_score-African_Nations_df$away_score

# Calcul de la somme par groupe
Avg_of_GA_per_tournament <- aggregate(GA ~ tournament, data = African_Nations_df, FUN = mean)

# Affichage du résultat
print(Avg_of_GA_per_tournament[order(Avg_of_GA_per_tournament$GA,decreasing = TRUE),])
