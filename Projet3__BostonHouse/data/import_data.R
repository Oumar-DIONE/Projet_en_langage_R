source(file = 'configue_S3_connexion.R')
my_minio<-retrieve_minio()

show_bucket_names<-function(minio=my_minio)
  
{
  my_buckets<-my_minio$list_buckets()$Buckets
  n=length(my_buckets)
  if (n>0)
  {
    for (i in 1:n)
    { 
    print(paste("mon bucket numéro ",i, "est: ",my_buckets[[i]]$Name))
    }
  }
  else 
  {
    print("mon bucket est actuellement vide")
  }
  
}

show_bucket_names()

download_data_from_S3<-function(data_local_path="'boston_data.csv'",data_path_in_S3='Boston_House_price/Boston-house-price-data.csv',bucket_name='odione',minio_object=my_minio)
{
  reponse<- minio_object$get_object(Bucket =bucket_name ,Key =data_path_in_S3 )
  writeBin(reponse$Body,data_local_path )
  print(paste("Téléchargement ,depuis S3, réussi et données situées localment à ", download_path))
}