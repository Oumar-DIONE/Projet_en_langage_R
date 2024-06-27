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

fill_nan<-function(data)
{
  col_name=get_colnames(data)
  l=length(col_name)
  for(i in 1:l) 
  {
    if (typeof(data[[col_name[i]]])=='character' | typeof(data[[col_name[i]]])=='integer')
      {
        fill_value=mode(data[[col_name[i]]])
        na.fill(data[[col_name[i]]],fill_value)
      }
    else if  (typeof(data[[col_name[i]]])=='double')
      {
      fill_value=mean(data[[col_name[i]]])
      na.fill(data[[col_name[i]]],fill_value)
     }
    
  }
  return(colSums(is.na(data)))
  
}

