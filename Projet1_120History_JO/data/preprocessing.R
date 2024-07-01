

binarizer<-function(data,char_var)
{
  var_name=paste0(char_var,"_num")
  data[[var_name]]=1
  value1<-unique(data[[char_var]])[1]
  value2<-unique(data[[char_var]])[2]
  if (char_var %in% names(data))
  {
    data[data[[char_var]]==value1,var_name]=1
    data[data[[char_var]]==value2,var_name]=0
    data<-data[,!names(data) %in% c(char_var)]
  }
  
  return(data)
}
# AVEC cette fonction, on va essayer de prédire le medail à partir des variables telles:
# "Medal","Sex", "Age","Height","Weight","Season"

preproc1<-function(data)
{
  df <- data[, c("Medal","Sex", "Age","Height","Weight","Season")]
  df$category_Medal <- as.numeric(as.factor(df$Medal))
  df<-binarizer(df,"Sex")
  df<-binarizer(df,"Season")
  df<-df[,!names(df) %in% c('Medal')]
  return(df)
  
}
model1_work_df<-preproc1(perfect_data[perfect_data$Year>2010,])

prepocess2<-function(data)
{
  df <- data[, c("Medal","Sex", "Age","Height","Weight","Season")]
  df$category_Medal <- as.numeric(as.factor(df$Medal))
  df<-binarizer(df,"Sex")
  df<-binarizer(df,"Season")
  df<-df[,!names(df) %in% c('Medal')]
  return(df)
  
}