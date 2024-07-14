data_dir="/home/onyxia/work/Projet_en_langage_R/Projet3__BostonHouse/data/"
boston_file<-"boston_data.csv"

get_data<-function(Data_dir=data_dir,file_name=boston_file)
{
  df<-read.csv(paste0(data_dir,boston_file))
  str(df)
  # le nombre total de valeur manquante
  print(paste0("le nombre total de valeur manquante est ", sum(is.null(df))))
  
  print(paste0("le nombre total de valeur manquante par colonnes ",sapply(df, function(x) sum(is.null(x)))))
  
  unname(sapply(df, function(x) is.character(x)))
  return(df)
 
}

mode_fun <- function(x) {
  # Calculer la fréquence de chaque valeur unique
  freq <- table(x)
  
  # Trouver la valeur la plus fréquente
  mode_value <- names(freq)[which.max(freq)]
  
  # Retourner le mode en respectant le type de la variable d'origine
  if (is.numeric(x)) {
    return(as.numeric(mode_value))
  } else if (is.integer(x)) {
    return(as.integer(mode_value))
  } else {
    return(mode_value)
  }
}

encoder<-function(df,cols_to_encode)
{
  n<-length(cols_to_encode)
  if(n>0)
  {
    for(i in 1:n)
    {
      col_<-cols_to_encode[i]
      p<-length(unique(df[[col_]]))
      print(paste(("p ",p)))
      df[[col_]]<-factor(df[[col_]],levels = 1:p)
    }
      
      
  }
  print("encodage bien fait!")
  return(df)
}
normalizator<-function(df, cols_to_norm)
{
  n<-length(cols_to_norm)
  if(n>0)
  {
    for(i in 1:n)
    {
      col_<-cols_to_norm[i]
      min_<-min(df[[col_]])
      max_<-max(df[[col_]])
      df[[col_]]<-(df[[col_]]-min_)/(max_-min_+0.0001)
    }
    
    
  }
  print("Normalisation bien fait!")
  return(df)
  
}
encoder(boston_data,names(boston_data)[unname(sapply(boston_data, function(x) is.double(x)))])

fillNa_fun <- function(x)
{
  if (is.double(x)) 
    {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    }
  else if( is.factor(x) | is.numeric(x))
   {
    mode_value<-mode_fun(x)
    x[is.na(x)] <- mode_value
   }
  return(x)
}

fillNull_fun <- function(x)
{
  if (is.double(x)) 
  {
    x[is.null(x)] <- mean(x, na.rm = TRUE)
  }
  else if( is.factor(x) | is.numeric(x))
  {
    mode_value<-mode_fun(x)
    x[is.null(x)] <- mode_value
  }
  return(x)
}

fillInfinity_fun <- function(x)
{
  if (is.double(x)) 
  {
    x[is.infinite(x)] <- mean(x, na.rm = TRUE)
  }
  else if( is.factor(x) | is.numeric(x))
  {
    mode_value<-mode_fun(x)
    x[is.infinite(x)] <- mode_value
  }
}


managed_undesired_value<-function(df)
{
  cols_<-unname(names(df))
  for( name_ in cols_)
  {
    df[[name_]]<-fillNa_fun(df[[name_]])
    df[[name_]]<-fillInfinity_fun(df[[name_]])
    df[[name_]]<-fillNull_fun(df[[name_]])
  }
  return(df)
}

preporcess_data<-function(df)
  
{ 
  # recupérer les colonnes de type integer puiq de type doucble
  cols_to_encode_=names(boston_data)[unname(sapply(boston_data, function(x) is.integer(x)))]
  cols_to_norm_=names(boston_data)[unname(sapply(boston_data, function(x) is.double(x)))]
  df1<-encoder(df,cols_to_encode = cols_to_encode_)
  df2<-normalizator(df1,cols_to_norm = cols_to_norm_)
  df3<-managed_undesired_value(df2)
  print("prétraitement bien fait!")
  return(df3)
  
}

boston_data<-get_data()
boston_transformed_data<-preporcess_data(boston_data)
  # ** Taches
# 1) convertir les types int en factors
# 2) normaliser les variables réelles avec la normalisations min-Max si la variables est une variable de mesures positives
# 3)checker s'il y' a des valeurs Null, nan , infity (aberrantes) puis soit supprimer la ligne correspondant soit la remplace la valeur
#manquante par la moyenne pour numericla variables ou par le mode pour les factors

