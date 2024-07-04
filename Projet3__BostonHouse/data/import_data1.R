tryCatch({
  # Créer un vecteur avec les données d'accès AWS
  vect <- list(
    AWS_ACCESS_KEY_ID = "67BZ4Z4V04R3KMDKQYSO",
    AWS_SECRET_ACCESS_KEY = "PZOO83OS2Fqi+NiBagVmQ7kzUFFGsmUKXdmgJBED",
    AWS_DEFAULT_REGION = "us-east-1",
    AWS_SESSION_TOKEN = "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhY2Nlc3NLZXkiOiI2N0JaNFo0VjA0UjNLTURLUVlTTyIsImFsbG93ZWQtb3JpZ2lucyI6WyIqIl0sImF1ZCI6WyJtaW5pby1kYXRhbm9kZSIsIm9ueXhpYSIsImFjY291bnQiXSwiYXV0aF90aW1lIjoxNzIwMDA2MDI5LCJhenAiOiJvbnl4aWEiLCJlbWFpbCI6Im91bWFyLmRpb25lQGVuc2FlLmZyIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImV4cCI6MTcyMDYxMDg1MSwiZmFtaWx5X25hbWUiOiJESU9ORSIsImdpdmVuX25hbWUiOiJPdW1hciIsImdyb3VwcyI6WyJVU0VSX09OWVhJQSJdLCJpYXQiOjE3MjAwMDYwNTAsImlzcyI6Imh0dHBzOi8vYXV0aC5sYWIuc3NwY2xvdWQuZnIvYXV0aC9yZWFsbXMvc3NwY2xvdWQiLCJqdGkiOiI3Nzk5NjhlMy02NWY3LTQ0ZTQtYTQ1NC03ZjIwMTNjNTk2NDMiLCJuYW1lIjoiT3VtYXIgRElPTkUiLCJwb2xpY3kiOiJzdHNvbmx5IiwicHJlZmVycmVkX3VzZXJuYW1lIjoib2Rpb25lIiwicmVhbG1fYWNjZXNzIjp7InJvbGVzIjpbIm9mZmxpbmVfYWNjZXNzIiwidW1hX2F1dGhvcml6YXRpb24iLCJkZWZhdWx0LXJvbGVzLXNzcGNsb3VkIl19LCJyZXNvdXJjZV9hY2Nlc3MiOnsiYWNjb3VudCI6eyJyb2xlcyI6WyJtYW5hZ2UtYWNjb3VudCIsIm1hbmFnZS1hY2NvdW50LWxpbmtzIiwidmlldy1wcm9maWxlIl19fSwic2NvcGUiOiJvcGVuaWQgcHJvZmlsZSBncm91cHMgZW1haWwiLCJzZXNzaW9uX3N0YXRlIjoiODMxZGNjNzctODA3Zi00MzllLWJlZDUtZWIwNDgwN2UzMzU1Iiwic2lkIjoiODMxZGNjNzctODA3Zi00MzllLWJlZDUtZWIwNDgwN2UzMzU1Iiwic3ViIjoiMWE2MjA5ZTgtOGE5MS00YmQyLWFmMDEtYzI2MDVjZTZlZWRjIiwidHlwIjoiQmVhcmVyIn0.WMjRAD68u_9ZsjPKBsTuqjLRtKFLiPxySqdZTqXWk8lmMgO82IwV55vaHsBOHlPC-hCSQ2aXlhSFA-cE-xvn4Q",
    AWS_S3_ENDPOINT = "minio.lab.sspcloud.fr"
  )
  
  # Afficher le vecteur pour vérifier son contenu
  print(vect)
  
  # Configuration de paws avec les informations du vecteur
  minio1 <- paws::s3(config = list(
    credentials = list(
      creds = list(
        access_key_id = vect$AWS_ACCESS_KEY_ID,
        secret_access_key = vect$AWS_SECRET_ACCESS_KEY,
        session_token = vect$AWS_SESSION_TOKEN
      )
    ),
    endpoint = paste0("https://", vect$AWS_S3_ENDPOINT),
    region = vect$AWS_DEFAULT_REGION
  ))
  
  # Lister les buckets
  buckets1 <- minio1$list_buckets()
  
  # Afficher les noms des buckets
  print(buckets1$Buckets)
}, error = function(e) {
  message("Error: ", conditionMessage(e))
})
