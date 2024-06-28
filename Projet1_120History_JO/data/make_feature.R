#install.packages("zoo")
library("zoo")

get_colnames<-function(data)
{
  col_name<-colnames(data)
  return(class(col_name))
}

check_nan<-function(data)
  {
  col_names==get_colnames((data))
  a=colSums(is.na(data))
  df=data.frame(a,row.names =col_names )
  return(a)
  
}
# Fonction pour calculer le mode
get_mode <- function(v) {
  v <- v[!is.na(v)]  # Retirer les NA
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Fonction pour obtenir les noms des colonnes
get_colnames <- function(df) {
  return(names(df))
}

# Fonction pour calculer le mode
get_mode <- function(v) {
  v <- v[!is.na(v)]
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

# Fonction pour remplir les valeurs manquantes
fill_nan <- function(df) {
  Col_names <- get_colnames(df)
  
  for (a in Col_names) {
    if (is.character(df[[a]]) || is.factor(df[[a]]) || is.integer(df[[a]])) {
      fill_value <- get_mode(df[[a]])
      df[[a]] <- na.fill(df[[a]], fill_value)
    } else if (is.numeric(df[[a]])) {
      fill_value <- mean(df[[a]], na.rm = TRUE)
      df[[a]] <- na.fill(df[[a]], fill_value)
    }
    print(fill_value)
  }
  
  return(df)  # Retourner le dataframe modifié
}

drop_

