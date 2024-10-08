fun.prime <- function(x,case=1){
  switch(case,  
         1,
         x*sqrt(2),
         sqrt(3*sqrt(3))*(1-x^2)*exp(-(x^2)/2),
         sqrt(3)*(sin(pi*x/2)+(x*pi/2)*cos(pi*x/2)),
         4*exp(-abs(x))*ifelse(x>0,-1,1) , 
         ifelse(x>1,1/x,-1/(x-2))*sqrt(10/3),
         sqrt(2)*( cos(pi*x)-pi*x*sin(pi*x)),(ifelse(x>1,1/x,-1/(x-2))*sqrt(10/3)- 0.6+ (x^2)*6)/8 )
  
}
x<-c(0,sqrt(2),sqrt,0)
fun.prime(0,case=6)/sqrt(10/3)
for(i in 1:8)
{
  print(fun.prime(0,case=i))
}
