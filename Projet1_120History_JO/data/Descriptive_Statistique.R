# Installer et charger GGally si nécessaire
#install.packages("GGally")
library(GGally)

# Exemple de graphique de paires avec GGally

matrix_corr<-function(data,feature1, feature2)
{
  
  data1=data[[feature1]]
  data2=data[[feature2]]
  df=data.frame(d1=data1,d2=data2)
  names(df)=c(feature1,feature2)
  ggpairs(df, aes( alpha = 0.5))
  
}