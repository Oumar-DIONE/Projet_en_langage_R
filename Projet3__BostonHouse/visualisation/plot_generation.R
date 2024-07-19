library("ggplot2")
par(mfrow=c(1,2))
plot(boston_transformed_data$AGE,boston_transformed_data$MEDV,type="p",col="red",main=" titre:prix en fun de age du bat")  # points
plot(boston_transformed_data$AGE,boston_transformed_data$MEDV,type="l",col="blue",main=" titre:prix en fun de age du bat")  # ligne)

plot_num_two_variable<-function(df,x_name,y_name,type_="p",title_="titre",color_="red")
{
  plot(df[[x_name]],df[[y_name]],type=type_,col=color_,main=title_,xlab=x_name,ylab=y_name)  # points
  
}

plot(boston_transformed_data$RAD)
hitogramme_for_factor_var<-function(df=boston_transformed_data,factor_col="RAD",color_="blue")
{
  plot(df[[factor_col]],col=df[["CHAS"]])

  
}

par(mfrow=c(2,1))
plot(boston_transformed_data$CHAS,boston_transformed_data$MEDV)

numerical_columns_boston_data<-names(boston_transformed_data)[unname(sapply(boston_transformed_data, function(x) is.double(x)))]
  
factor_columns_boston_data<-names(boston_transformed_data)[unname(sapply(boston_transformed_data, function(x) is.factor(x)))]

# install.packages("latex2exp")
library(latex2exp)
plot(boston_transformed_data[numerical_columns_boston_data],main = TeX("$\\beta^3, \\beta \\in 1 \\ldots 10$"))
is.data.frame(boston_transformed_data[numerical_columns_boston_data])

par(mfrow=c(6,4))
par(mar = c(1, 1, 1, 1)) # le marges

for (cat_col in factor_columns_boston_data)
{ 
  print(paste("cat_col ",cat_col))
  for (num_col in numerical_columns_boston_data)
    print(paste("num_col ",num_col))
    plot(boston_transformed_data[[cat_col]],boston_transformed_data[[num_col]])
}

# detail on titlr argument
plot(boston_transformed_data$CHAS,boston_transformed_data$MEDV,xlab="Binari_variable",ylab="median-price")

plot(boston_transformed_data$CHAS,boston_transformed_data$MEDV,axes=FALSE) # sans axes



