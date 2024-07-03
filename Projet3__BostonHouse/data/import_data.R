tryCatch({
  # Charger le fichier YAML
  config <- yaml::yaml.load_file(paste0(Boston_data_path, "config2.yaml"))
  
  # Configuration de paws avec les informations du fichier YAML
  minio <- paws::s3(config = list(
    credentials = list(
      creds = list(
        access_key_id = config$AWS_ACCESS_KEY_ID,
        secret_access_key = config$AWS_SECRET_ACCESS_KEY,
        session_token = config$AWS_SESSION_TOKEN
      )
    ),
    endpoint = paste0("https://", config$AWS_S3_ENDPOINT),
    region = config$AWS_DEFAULT_REGION
  ))
  
  # Lister les buckets
  buckets <- minio$list_buckets()
  
  # Afficher les noms des buckets
  print(buckets$Buckets)
}, error = function(e) {
  message("Error: ", conditionMessage(e))
})
