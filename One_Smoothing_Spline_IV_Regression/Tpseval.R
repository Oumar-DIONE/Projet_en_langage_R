knots_<-1:n
a_<-rep(1,2)
d_<-seq(0,8,length=n)
tpseval <- function(t,knots,a=a_,d=d_)
{
  cub <- (abs(t-knots)^3)/12
  c(1,t)%*%a + cub%*%d
}

n<-4
d_<-rep(1,n)
T_<-seq(0,10,length=9)
for(j in 1:length(T_))
{
  t<-T_[j]
  value<-12*tpseval(t,knots = knots_,d=d_)
  print(paste("g de ", t, "est égale à ",value))
}


tpseval.prime <- function(t,knots,a=a_,d=d_){ ## This is the function returning the first derivative of ghat.iv
  ## This is written in the same logic as the tps function above
  sqsign <- (3/12)*((t-knots)^2)*ifelse(t-knots>0,1,-1)
  a[2]+sqsign%*%d
}

for(j in 1:length(T_))
{
  t<-T_[j]
  value<-4*tpseval.prime(t,knots = knots_,d=d_)
  print(paste("g prime de ", t, "est égale à ",value))
}
# j'ai multiplié par 4 4 pour avoir des résulats ronds mais le 4 ne fait pas partie de l'expression de g
#prime j'ai fait la même chose pour g aussi avec lav aleur122'