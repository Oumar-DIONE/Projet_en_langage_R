library(ggplot2)
#getwd()
#setwd("/home/onyxia/work/Projet_en_langage_R/Projet1_120History_JO/Outputs")

## data contient les valeurs d'une seule colonne 
hist_single_var<-function(data,x = "Valeurs",
                          y = "Fréquence",binwidth = 10, fill = "blue", color = "black", alpha = 0.7)
{
  df <- data.frame(value = data,col_names=x)
  
  # Tracer l'histogramme avec ggplot2
  ggplot(df, aes(x = data)) +
    geom_histogram(binwidth = binwidth, fill = fill, color = color, alpha = alpha) +
    labs(title = "Histogramme des valeurs",
         x = x,
         y = "Fréquence")
  
}

# la fonction ci-dessous permet de tracer l'histogramme des données data et découpe chaque bart en 
# selon la distribution de la colonne category_data dans l'intervalle de valeur corespondant.'

hist_var<-function(data,category_data,x = "Valeurs",cat_var_name="group var",
                          y = "Fréquence",bins = 10, fill = "blue", alpha = 0.7)
{
  df <- data.frame(value = data,group=category_data)
  title=paste("Histogramme des ",x,"selon le ou la ",cat_var_name)
  group_name=unique(category_data)
  l=length(group_name)
  b=l+10
  color_name=colors()[10:b]
  # Crée une liste de couleurs nommée
  color_mapping <- setNames(color_name, group_name)
  
  # Tracer l'histogramme avec ggplot2
  ggplot(df, aes(x = value,fill = group)) +
    geom_histogram(bins = bins, alpha = alpha) +scale_fill_manual(values = color_mapping) +  # Définir les couleurs pour chaque groupe+
    labs(title = title,x = x,y = "Fréquence")
  
}