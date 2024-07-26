# Chargement des bibliothèques
library(paws)
library(yaml)

retrieve_minio <- function(file_path = "/home/onyxia/work/Projet_en_langage_R/Projet3__BostonHouse/configuration/config.yaml") {
  # Lire le fichier YAML
  config <- yaml.load_file(file_path)
  print("fichier yaml bien chargé")
  
  # Extraire les valeurs du fichier YAML et les assigner à des variables
  AWS_ACCESS_KEY_ID <- config$AWS_ACCESS_KEY_ID
  AWS_SECRET_ACCESS_KEY <- config$AWS_SECRET_ACCESS_KEY
  AWS_SESSION_TOKEN <- config$AWS_SESSION_TOKEN
  AWS_DEFAULT_REGION <- "us-east-1"
  AWS_S3_ENDPOINT <- "minio.lab.sspcloud.fr"
  
  Sys.setenv(
    AWS_ACCESS_KEY_ID = AWS_ACCESS_KEY_ID,
    AWS_SECRET_ACCESS_KEY = AWS_SECRET_ACCESS_KEY,
    AWS_DEFAULT_REGION = AWS_DEFAULT_REGION,
    AWS_SESSION_TOKEN = AWS_SESSION_TOKEN,
    AWS_S3_ENDPOINT = AWS_S3_ENDPOINT
  )
  
  minio <- paws::s3(config = list(
    credentials = list(
      creds = list(
        access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
        secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
        session_token = Sys.getenv("AWS_SESSION_TOKEN")
      )),
    endpoint = paste0("https://", Sys.getenv("AWS_S3_ENDPOINT")),
    region = Sys.getenv("AWS_DEFAULT_REGION")
  ))
  
  print('connexion à S3 reussie!')
  return(minio)
}

# Exemple d'utilisation
# Assurez-vous que le fichier config.yaml est au bon endroit et contient les bonnes informations
# minio <- retrieve_minio("path/to/your/config.yaml")
