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

encoder <- function(data_, cols_to_encode) {
  # Créer une copie des données
  df <- data_
  
  # Vérifier que les colonnes spécifiées existent dans le DataFrame
  missing_cols <- setdiff(cols_to_encode, colnames(df))
  if (length(missing_cols) > 0) {
    stop(paste("Les colonnes suivantes ne sont pas présentes dans le DataFrame:", paste(missing_cols, collapse = ", ")))
  }
  
  # Si il y a des colonnes à encoder
  if (length(cols_to_encode) > 0) {
    # Convertir les colonnes spécifiées en facteurs
    df[cols_to_encode] <- lapply(df[cols_to_encode], as.factor)
  }
  
  # Message de confirmation
  print("Encodage bien fait!")
  
  # Retourner le DataFrame modifié
  return(df)
}


normalizator <- function(df, cols_to_norm) {
  # Nombre de colonnes à normaliser
  n <- length(cols_to_norm)
  
  if (n > 0) 
    {
    for (col_name in cols_to_norm) 
      {
      # Calculer le minimum et le maximum de la colonne
      col_min <- min(df[[col_name]], na.rm = TRUE)
      col_max <- max(df[[col_name]], na.rm = TRUE)
      if (col_max>1 & col_min>-1)  # # we only normalise which have value out side of (-,1)
        {
        # Eviter la division par zéro
        if (col_min != col_max) {
          # Normaliser la colonne
          df[[col_name]] <- (df[[col_name]] - col_min) / (col_max - col_min)
        } else {
          warning(paste("La colonne", col_name, "a une valeur constante et ne peut pas être normalisée."))
          df[[col_name]] <- 0.5 # Par exemple, on peut assigner une valeur constante comme 0.5
        }
      
      
      }
    }
  }
  
  print("Normalisation bien faite!")
  return(df)
}



fillNa_fun <- function(x)
{ 
  if (sum(is.na(x))>0)
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
  }
  
  return(x)
}

fillNull_fun <- function(x)
{ 
  if (sum(is.null(x))>0)
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
    
  }
  
  return(x)
}

fillInfinity_fun <- function(x)
{
  if(sum(is.infinite(x)>0))
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
  return(x)

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
  cols_to_encode_=names(df)[unname(sapply(df, function(x) is.integer(x)))]
  cols_to_norm_=names(df)[unname(sapply(df, function(x) is.double(x)))]
  df1<-encoder(df,cols_to_encode = cols_to_encode_)
  df2<-normalizator(df1,cols_to_norm = cols_to_norm_)
  df3<-managed_undesired_value(df2)
  print("prétraitement bien fait!")
  return(df3)
  
}

boston_data<-get_data()
boston_transformed_data<-preporcess_data(boston_data)
  
