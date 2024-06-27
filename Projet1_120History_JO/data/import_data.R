data_dir<-"/home/onyxia/work/Projet_en_langage_R/Projet1_120History_JO/data/"
file_name<-"athlete_events.csv"
load_data<-function(file_name)
{ 
  path_name<-paste0(data_dir,file_name)
  data<- read.csv(path_name)
  return(data)
}
raw_data<-load_data(file_name)