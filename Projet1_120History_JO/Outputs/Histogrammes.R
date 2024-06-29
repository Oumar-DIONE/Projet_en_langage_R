library(ggplot2)
#getwd()
#setwd("/home/onyxia/work/Projet_en_langage_R/Projet1_120History_JO/outputs")

hist_single_var<-function(data,x = "Valeurs",
                          y = "Fréquence",binwidth = 10, fill = "blue", color = "black", alpha = 0.7)
{
  df <- data.frame(value = data)
  
  # Tracer l'histogramme avec ggplot2
  ggplot(df, aes(x = data)) +
    geom_histogram(binwidth = binwidth, fill = fill, color = color, alpha = alpha) +
    labs(title = "Histogramme des valeurs",
         x = x,
         y = "Fréquence")
  
}