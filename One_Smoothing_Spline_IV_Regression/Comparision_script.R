# case=1
d1<-as.vector(as.matrix(read.csv("Aa8biais_n_equal_100.csv")))
d2<-as.vector(as.matrix(read.csv("Aa8biais_n_equal_200.csv")))
d3<-as.vector(as.matrix(read.csv("Aa8biais_n_equal_300.csv")))
d4<-as.vector(as.matrix(read.csv("Aa8biais_n_equal_400.csv")))
data_case1<-rbind(n_100=d1,n_200=d2,n_300=d3,n_400=d4)
colnames(data_case1)<-c("biasiv","biastik","biasgal")
print(data_case1)

# case=2

e1<-as.vector(as.matrix(read.csv("Ab8biais_n_equal_100.csv")))
e2<-as.vector(as.matrix(read.csv("Ab8biais_n_equal_200.csv")))
e3<-as.vector(as.matrix(read.csv("Ab8biais_n_equal_300.csv")))
e4<-as.vector(as.matrix(read.csv("Ab8biais_n_equal_400.csv")))
data_case2<-rbind(n_100=e1,n_200=e2,n_300=e3,n_400=e4)
colnames(data_case2)<-c("biasiv","biastik","biasgal")
print(data_case2)


# case=3

f1<-as.vector(as.matrix(read.csv("Ac8biais_n_equal_100.csv")))
f2<-as.vector(as.matrix(read.csv("Ac8biais_n_equal_200.csv")))
f3<-as.vector(as.matrix(read.csv("Ac8biais_n_equal_300.csv")))
f4<-as.vector(as.matrix(read.csv("Ac8biais_n_equal_400.csv")))
data_case2<-rbind(n_100=f1,n_200=f2,n_300=f3,n_400=f4)
colnames(data_case2)<-c("biasiv","biastik","biasgal")
print(data_case2)
print("Conclusion fo case 3: Nous voyons que le bias decroit , quelques soit la meethode d'estimation selon la taille de l'echantillon ce qui n'est pas suprenant" )
 

retrieve_conclusion<-function(data_case)
 {
   p1<-nrow(data_case)
   p2<-ncol(data_case)
   for (c in 1:p2)
   {
     values<-data_case[1:p1,c]
     col_name<-colnames(data_case)[c]
     dif<-diff(values)
     bool_value<-all(dif<=0)
     if (bool_value==TRUE)
      { print(paste("le bias decroit avec la taille pour la methode",col_name))} 
     else if (bool_value==FALSE)
     {print(paste("Probleme: le bias ne decroit pas avec la taille pour la methode",col_name))
       print(values)}
       
   }
 }
 
retrieve_conclusion(data_case1)
colnames(data_case1)[1]
